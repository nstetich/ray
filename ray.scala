import scala.math._

object PrettyPrinter {
  def print(objects:AnyRef*) {
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

  def +(that:Vector3) = new Vector3(this.x + that.x, this.y + that.y, this.z + that.z)
  def -(that:Vector3) = new Vector3(this.x - that.x, this.y - that.y, this.z - that.z)
  def *(s:Double) = new Vector3(x * s, y * s, z * s)

  def dot(that:Vector3) = this.x * that.x + this.y * that.y + this.z * that.z
  def cross(that:Vector3) = new Vector3(
    this.y * that.z + this.z * that.y, 
    this.z * that.x + this.x * that.z,
    this.x * that.y + this.y * that.x
  )

  override def toString = "(" + x + ", " + y +  ", " + z + ")"

}

class Point2(val x:Double, val y:Double) {

}

class Point3(val x:Double, val y:Double, val z:Double) {
  override def toString = "(" + x + ", " + y +  ", " + z + ")"  
}
