import scala.math._

object PrettyPrinter {
  def coords(objects:Number*):String = {
    objects.mkString("(", ", ", ")")
  }
}

class Vector3(val x:Double, val y:Double, val z:Double) {

  def magnitude = sqrt(x * x + y * y + z * z)
  def direction = {
    val m = magnitude
    if (m == 0) {
      this
    } else {
      new Vector3(x / m, y / m, z / m)
    }
  }

  def +(v:Vector3) = new Vector3(x + v.x, y + v.y, z + v.z)
  def -(v:Vector3) = new Vector3(x - v.x, y - v.y, z - v.z)
  def *(s:Double) = new Vector3(x * s, y * s, z * s)

  def dot(v:Vector3) = x * v.x + y * v.y + z * v.z
  def cross(v:Vector3) = new Vector3(
    y * v.z + z * v.y, 
    z * v.x + x * v.z,
    x * v.y + y * v.x
  )

  override def toString = PrettyPrinter.coords(x, y, z)
}

class Point2(val x:Double, val y:Double) {
  override def toString = PrettyPrinter.coords(x, y)
}

class Point3(val x:Double, val y:Double, val z:Double) {
  override def toString = PrettyPrinter.coords(x, y, z)
}

class Color(val r:Double, val g:Double, val b:Double) {
  require(r >= 0 && r <= 1 && g >= 0 && g <= 1 && b >= 0 && b <= 1)
  override def toString = PrettyPrinter.coords(r, g, b)
}

