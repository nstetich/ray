import scala.util.parsing.combinator._
import scala.util.matching.Regex

object ModelParsers extends RegexParsers {
//  def colorComponent = "[0-9a-f]{2}".r ^^ (Integer.parseInt(_, 16))
//  def color = "#" ~> repN(3, colorComponent) ^^ 
//    { case List(r, g, b) => new Color(r, g, b) }

  val Optsep:Regex = ",?".r

  def ident = """[a-zA-Z_][a-zA-Z_0-9-]*""".r

//  def integer = """-?\d+""".r ^^ { case n => n.toInt }

  def decimal = """-?(\d+(\.\d+)?)|(\d*\.\d+)""".r ^^ { case n => n.toDouble }

  def color = "#"~>"[0-9a-fA-F]{1,6}".r ^^
    { case rgb => new Color(Integer.parseInt(rgb, 16)) }

  def tup3fpn = decimal~","~decimal~","~decimal ^^ { case a~","~b~","~c => (a, b, c) }
  
  def point3 = "("~>tup3fpn<~")" ^^ { case (x, y, z) => new Point3(x, y, z) }

  def vector3 = "<"~>tup3fpn<~">" ^^ { case (x, y, z) => new Vector3(x, y, z) }

  def vertex = point3~"^"~vector3 ^^ { case p~"^"~v => new Vertex(p, v) }

  def value:Parser[Any] = (
    // integer | 
    decimal | color | vertex | point3 | vector3 | obj )

  def valueList = "["~>repsep(value, Optsep)<~"]"

  def property = ident~"="~(value|valueList) ^^ { case n~"="~v => Tuple2(n, v) }

  def propertyList = "{"~repsep(property, Optsep)~"}" ^^ { case "{"~plist~"}" => Map() ++ plist }  

  def obj:Parser[Any] = ident~":"~propertyList ^^
    {
      case "point-light"~":"~props => 
        (props("location"), 
         props("intensity")) match {
          case (l:Point3, i:Color) => new PointLight(l, i)
        }
      case "screen"~":"~props =>
        (props("origin"), 
         props("xAxis"), 
         props("yAxis"), 
         props("xRes"), 
         props("yRes")) match {
           case (o:Point3, xAxis:Vector3, yAxis:Vector3, xRes:Double, yRes:Double) =>
             new Screen(o, xAxis, yAxis, xRes.asInstanceOf[Int], yRes.asInstanceOf[Int])
        }
      case "sphere"~":"~props =>
        (props("origin"), 
         props("radius"), 
         props("material")) match {
          case (o: Point3, r: Double, m: Material) => new Sphere(o, r, m)
        }
      case "triangle"~":"~props =>
        (props("vertex1"),
         props("vertex2"),
         props("vertex3"),
         props("material")) match {
          case(v1: Vertex, v2: Vertex, v3: Vertex, m: Material) => new Triangle(v1, v2, v3, m)
        }
      case "material"~":"~props =>
        (props("reflectance"), 
         props("highlight")) match {
          case (r:Color, h:Color) => new GenericMaterial(r, h)
        }
      case "model"~":"~props =>
        (props("eye"), 
         props("screen"), 
         props("lights"),
         props("surfaces"),
         props("ambient"), 
         props("bg")) match {
          // Argh, blasted type erasure... 
          case (e: Point3, sc: Screen, l: List[Light], su: List[Surface], a:Color, bg:Color) =>
            new Model(e, sc, l, su, a, bg)
        }
      case x => x
    }

}

object ModelParsersMain { 
  def main(args: Array[String]) : Unit = {
    implicit def asCharSequence(s: String) = s.asInstanceOf[CharSequence]
//    val p = ModelParsers.colorComponent
//    println(ModelParsers.parseAll(ModelParsers.colorComponent, "ab".asInstanceOf[CharSequence]))
    println(ModelParsers.parseAll(ModelParsers.color, "#abcdef".asInstanceOf[CharSequence]))
    println(ModelParsers.parseAll(ModelParsers.point3, "(1,2,3)".asInstanceOf[CharSequence]))
    println(ModelParsers.parseAll(ModelParsers.property, "madness = <3.0, 2.0, 1.0>".asInstanceOf[CharSequence]))
    println(ModelParsers.parseAll(ModelParsers.vertex, "(1,2,3) ^ <1,2,3>"))
    val m = """
model: {
  eye = (0, 0, 10)
  ambient = #404040
  bg = #000000
  screen = screen: {
		origin = (-1.5, -1.5, 4) 
		xAxis = <3, 0, 0>
		yAxis = <0, 3, 0> 
		xRes = 400, yRes = 400
  }
	lights = [
		point-light: { location = (-10, 10, 10), intensity = #ff0000 }
    point-light: { location = (10, 10, 10), intensity = #ffffff }
  ]
  surfaces = [
    sphere: { 
      origin = (0, 0, 0)
      radius = 1.0
      material = material: {
        reflectance = #ffffff
        highlight = #ffffff
      }
    }
    sphere: { 
      origin = (1, 1, 1)
      radius = 0.5
      material = material: {
        reflectance = #ffffff
        highlight = #ffffff
      }
    }
  ]
}
"""
    println(ModelParsers.parseAll(ModelParsers.obj ,m.asInstanceOf[CharSequence])) 
  }
}

object RenderTest {
  import java.io._

  def main(args:Array[String]) = {
    val inFilename = args(0)
    val outFilename = args(1)
    val input = new BufferedReader(new FileReader(inFilename))
    val output = new File(outFilename)
    try {
      ModelParsers.parseAll(ModelParsers.obj, input).get match {
        case model:Model => model.render(output)
        case x =>  {
          println("Received input other than a model.")
          println(x)
        }
      }
    } finally {
      if (input != null) 
        input.close()
    }
  }
}

