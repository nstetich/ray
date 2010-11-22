import scala.math._

object PrettyPrinter {
  def coords(objects:Number*) = {
    objects.mkString("(", ", ", ")")
  }
}

object Constants {
  val Error = 5E-10
  def floatCompare(a:Double, b:Double) = {
    val lowerBound = b - Error
    val upperBound = b + Error
    if (a < lowerBound) 
      -1
    else if (a >= lowerBound && a <= upperBound) 
      0
    else 
      1
  }
}

class Vector2(val x:Double, val y:Double) {
  def magnitude = sqrt(x * x + y * y)
  def direction = {
    val m = magnitude
    if (m == 0) this else new Vector2(x / m, y / m)
  }

  def +(v:Vector2) = new Vector2(x + v.x, y + v.y)
  def -(v:Vector2) = new Vector2(x - v.x, y - v.y)
  def *(s: Double) = new Vector2(x * s, y * s)
  def unary_- = new Vector2(-x, -y)

  def dot(v:Vector2) = x * v.x + y * v.y

  override def toString = "V" + PrettyPrinter.coords(x, y)
}

class Vector3(val x:Double, val y:Double, val z:Double) {

  def magnitude = sqrt(x * x + y * y + z * z)
  def direction = {
    val m = magnitude
    if (m == 0) this else new Vector3(x / m, y / m, z / m)
  }

  def +(v:Vector3) = new Vector3(x + v.x, y + v.y, z + v.z)
  def -(v:Vector3) = new Vector3(x - v.x, y - v.y, z - v.z)
  def *(s:Double) = new Vector3(x * s, y * s, z * s)
  def unary_- = new Vector3(-x, -y, -z)

  def dot(v:Vector3) = x * v.x + y * v.y + z * v.z
  // (a2b3 − a3b2, a3b1 − a1b3, a1b2 − a2b1).
  def cross(v:Vector3) = new Vector3(
    y * v.z - z * v.y, 
    z * v.x - x * v.z,
    x * v.y - y * v.x
  )

  override def toString = "V" + PrettyPrinter.coords(x, y, z)
}

object Vector3 {
  val Zero = new Vector3(0, 0, 0)
} 

class Point2(val x:Double, val y:Double) {
  override def toString = "P" + PrettyPrinter.coords(x, y)
}

class Point3(val x:Double, val y:Double, val z:Double) {
  def +(v:Vector3) = new Point3(x + v.x, y + v.y, z + v.z)
  def -(v:Vector3) = new Point3(x - v.x, y - v.y, z - v.z)
  def -(p:Point3) = new Vector3(x - p.x, y - p.y, z - p.z)
  override def toString = "P" + PrettyPrinter.coords(x, y, z)
}

class Ray(val origin:Point3, val vector:Vector3)

class Vertex(val location:Point3, val normal:Vector3)

class Intersection(
  val surface:Surface, 
  val t:Double
) extends Ordered[Intersection] {
  override def toString = String.format("surface = %s, t = %f", surface, t.asInstanceOf[AnyRef])
  override def compare(i:Intersection) = t.compare(i.t)
}

class Color(val r:Double, val g:Double, val b:Double) {
  require(r >= 0 && r <= 1 && g >= 0 && g <= 1 && b >= 0 && b <= 1)

  def this(r: Int, g: Int, b: Int) = {
    this(r.asInstanceOf[Double] / 255.0, 
         g.asInstanceOf[Double] / 255.0,
         b.asInstanceOf[Double] / 255.0)
  }
  def this(rgb: Int) = {
    this((rgb >> 16) & 0xff, (rgb >> 8) & 0xff, rgb & 0xff)
  }

  def *(c: Color) = new Color(r * c.r, g * c.g, b * c.b)
  def *(n: Double) = {
    require(n <= 1 && n >= 0)
    new Color(r * n, g * n, b * n)
  }
  def +(c: Color) = new Color(min(1, r + c.r), min(1, g + c.g), min(1, b + c.b))

  def intValue = {
    ((r * 255).asInstanceOf[Int] << 16) |
    ((g * 255).asInstanceOf[Int] << 8) |
     (b * 255).asInstanceOf[Int]
  }

  override def toString = String.format("Color: #%06x", intValue.asInstanceOf[AnyRef])
}

object Color {
  val White = new Color(1.0, 1.0, 1.0)
  val Black = new Color(0, 0, 0)
  val Red = new Color(1.0, 0, 0)
  val Blue = new Color(0, 0, 1.0)
  val Green = new Color(0, 1.0, 0)
  val Yellow = new Color(1.0, 1.0, 0)

  def apply(r: Double, g: Double, b: Double) = new Color(r, g, b)
  def apply(r: Int, g: Int, b: Int) = new Color(r, g, b)

  def hexString(c:Color) = String.format("#%08x", c.intValue.asInstanceOf[AnyRef])
}




