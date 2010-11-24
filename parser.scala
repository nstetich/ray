import scala.util.parsing.combinator._
import scala.util.matching.Regex

case class ModelObject(name:String, props:Map[String, Any])

case class Block(env: ModelParsers.Environment, obj: ModelObject)

class UndefinedVariableException(message:String, 
    cause:Throwable) extends Exception(message, cause) {
  def this(message:String) = this(message, null)
  def this(cause:Throwable) = this(null, cause)
  def this() = this(null, null)
}

object ModelParsers extends RegexParsers {

  type Environment = Map[String, Any]

  val Sep: Regex = ",?".r

  def ident: Parser[String] = """[a-zA-Z_][a-zA-Z_0-9-]*""".r

  def integer: Parser[Int] = """-?\d+""".r ^^ 
    { case n => n.toInt }

  def decimal: Parser[Double] = """-?(\d+(\.\d+)?)|(\d*\.\d+)""".r ^^ 
    { case n => n.toDouble }

  def color: Parser[Color] = "#" ~> "[0-9a-fA-F]{1,6}".r ^^
    { case rgb => new Color(Integer.parseInt(rgb, 16)) }

  def tup3fpn: Parser[Tuple3[Double, Double, Double]] = {
    decimal ~ "," ~ decimal ~ "," ~ decimal ^^ 
      { case a ~ "," ~ b ~ "," ~ c => (a, b, c) }
  }
  
  def point3: Parser[Point3] = "(" ~> tup3fpn <~ ")" ^^ 
    { case (x, y, z) => new Point3(x, y, z) }

  def vector3: Parser[Vector3] = "<" ~> tup3fpn <~ ">" ^^ 
    { case (x, y, z) => new Vector3(x, y, z) }

  def vertex: Parser[Vertex] = point3 ~ "^" ~ vector3 ^^ 
    { case p ~ "^" ~ v => new Vertex(p, v) }

  def value: Parser[Any] = ((decimal ||| integer) | 
    color | vertex | point3 | vector3 | (block ||| varref))

  def valueList: Parser[List[Any]] = "[" ~> repsep(value, Sep) <~ "]"

  def property: Parser[Tuple2[String, Any]] =  {
    ident ~ "=" ~ (value | valueList) ^^ 
      { case n ~ "=" ~ v => (n, v) }
  }

  def propertyList: Parser[Map[String, Any]] = {
    repsep(property, Sep) ^^ { case plist => Map() ++ plist }
  }

  def obj: Parser[ModelObject] = { 
    ident ~ ":" ~ "{" ~ propertyList ~ "}" ^^
      { case name ~ ":" ~ "{" ~ props ~ "}" => new ModelObject(name, props) }
  }

  def varref = ident

  def vardecl: Parser[Tuple2[String, Any]] = {
    ident ~ ":=" ~ (value | valueList) ^^ 
      { case n ~ ":=" ~ v => (n, v) }
  }

  def vardecls: Parser[Environment] = {
//    (repsep(vardecl, Sep) ?) ^^ 
//      { case Some(vardecls) => Map() ++ vardecls 
//        case None => Map() }
    repsep(vardecl, Sep) ^^ { case vardecls => Map() ++ vardecls }
  }

  def block: Parser[Block] = {
    vardecls ~ Sep ~ obj ^^
      { case vardecls ~ _ ~ obj => new Block(vardecls, obj) }
  }
}

class ModelEval(val parent:Option[ModelEval], val block:Block) {

  val AutoApplyPrefix = "_"

  def this(block:Block) = {
    this(None, block)
  }

  def eval(b: Block): Any = {
    new ModelEval(Some(this), b).eval()
  }

  def evalVar(varName: String): Option[Any] = {
    try {
      Some(evalValue(block.env(varName)))
    } catch {
      case e: NoSuchElementException => parent match {      
        case Some(p: ModelEval) => p.evalVar(varName)
        case _ => None
      }
    }
  }

  def evalValue(value: Any): Any = {
    value match {
      case s:String    => evalVar(s) match {
        case Some(s) => s
        case None => throw new UndefinedVariableException("Undefined variable!")
      }
      case l:List[Any] => l map {v => evalValue(v)}
      case b:Block     => eval(b)
      case x => x
    }
  }

  def evalProp(propName: String): Any = {
    val property = try {
      // First see if the property is defined on the object
      block.obj.props(propName)
    } catch {
      // Try looking for the property in the variables defined
      // for the block.
      case e:NoSuchElementException => 
        try {
          evalValue(AutoApplyPrefix + propName)
        } catch {
          // If the variable lookup fails, throw the original exception
          case _: UndefinedVariableException => throw e
        }
    }
    evalValue(property)
  }

  def evalProps(propNames: String*): Seq[Any] = {
    propNames.map(evalProp(_))
  }

  def eval(): Any = {
    block.obj.name match {
      case "point-light" =>
        evalProps("location", "intensity") match {
          case Seq(l:Point3, i:Color) => new PointLight(l, i)
        }
      case "screen" =>
        evalProps("origin", "xAxis", "yAxis", "xRes", "yRes") match {
          case Seq(o:Point3, xAxis: Vector3, yAxis: Vector3, xRes: Int, yRes: Int) =>
            new Screen(o, xAxis, yAxis, xRes, yRes)
        }
      case "sphere" => 
        evalProps("origin", "radius", "material") match {
          case Seq(o: Point3, r: Double, m: Material) => new Sphere(o, r, m)
        }
      case "triangle" =>
        evalProps("vertex1", "vertex2", "vertex3", "material") match {
          case Seq(v1: Vertex, v2: Vertex, v3: Vertex, m:Material) =>
            new Triangle(v1, v2, v3, m)
        }
      case "material" =>
        evalProps("reflectance", "reflectivity", "highlight") match {
          case Seq(rc: Color, rv:Color, h: Color) => new GenericMaterial(rc, rv, h)
        }
      case "model" =>
        evalProps("eye", "screen", "lights", 
          "surfaces", "ambient", "bg", "recursion-depth") match {
          case Seq(e: Point3, sc: Screen, li: List[Light], 
                    su: List[Surface], amb: Color, bg: Color, rd: Int) =>
            new Model(e, sc, li, su, amb, bg, rd)
        }
    }
  }
}

object ModelEval {
  def eval(block: Block) = new ModelEval(block).eval()
}

object ModelParsersMain { 
  def main(args: Array[String]) : Unit = {
    implicit def asCharSequence(s: String) = s.asInstanceOf[CharSequence]
    println(ModelParsers.parseAll(ModelParsers.color, "#abcdef".asInstanceOf[CharSequence]))
    println(ModelParsers.parseAll(ModelParsers.point3, "(1,2,3)".asInstanceOf[CharSequence]))
    println(ModelParsers.parseAll(ModelParsers.property, "madness = <3.0, 2.0, 1.0>".asInstanceOf[CharSequence]))
    println(ModelParsers.parseAll(ModelParsers.vertex, "(1,2,3) ^ <1,2,3>"))
    println(ModelParsers.parseAll(ModelParsers.obj, "foo: { bar = 2, baz = #123456 }"))
    println(ModelParsers.parseAll(ModelParsers.block, "foo: { bar = 2, baz = #123456 }"))
  }
}

object RenderTest {
  import java.io._

  def badInput(n:String, x: Any) = {
    println("Received input other than a " + n)
    println(x)
  }

  def main(args:Array[String]) = {
    val inFilename = args(0)
    val outFilename = args(1)
    val input = new BufferedReader(new FileReader(inFilename))
    val output = new File(outFilename)
    try {
      ModelParsers.parseAll(ModelParsers.block, input).get match {
        case (block: Block) => { 
          ModelEval.eval(block) match {      
            case model: Model => model.render(output)
            case x => badInput("model", x)
          }
        }
        case x =>  badInput("block", x)
      }
    } finally {
      if (input != null) 
        input.close()
    }
  }
}

