import scala.math._

object PrettyPrinter {
  def coords(objects:Number*) = {
    objects.mkString("(", ", ", ")")
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

  override def toString = PrettyPrinter.coords(x, y)
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

  override def toString = PrettyPrinter.coords(x, y, z)
}

object Vector3 {
  val Zero = new Vector3(0, 0, 0)
} 

class Point2(val x:Double, val y:Double) {
  override def toString = PrettyPrinter.coords(x, y)
}

class Point3(val x:Double, val y:Double, val z:Double) {
  def +(v:Vector3) = new Point3(x + v.x, y + v.y, z + v.z)
  def -(v:Vector3) = new Point3(x - v.x, y - v.y, z - v.z)
  override def toString = PrettyPrinter.coords(x, y, z)
}

class Color(val r:Double, val g:Double, val b:Double) {
  require(r >= 0 && r <= 1 && g >= 0 && g <= 1 && b >= 0 && b <= 1)

  def intValue = {
    ((r * 255).asInstanceOf[Int] << 16) |
    ((g * 255).asInstanceOf[Int] << 8) |
    (b * 255).asInstanceOf[Int]
  }

  override def toString = PrettyPrinter.coords(r, g, b)
}

// Surfaces: move to another file?

trait Surface {
  def normal(p:Point3) = Vector3.Zero // default implementation
}

class Sphere(val center:Point3, val r:Double) extends Surface {

}

class Triangle(val p1: Point3, val p2: Point3, val p3:Point3) extends Surface {

}

class Screen(val origin:Point3, val xAxis:Vector3, val yAxis:Vector3, 
    val w:Int, val h:Int) {
  def normal = (xAxis cross yAxis).direction

  def pixelAt(x:Double, y:Double) = 
    origin + xAxis * ((x + 0.5) / w)  + yAxis * ((y + 0.5) / h)
}
