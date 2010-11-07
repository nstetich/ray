import scala.math._

object PrettyPrinter {
  def coords(objects:Number*) = {
    objects.mkString("(", ", ", ")")
  }
}

object Constants {
  val Error = 0.000005
  def floatCompare(a:Double, b:Double) = {
    val lowerBound = b - Error
    val upperBound = b + Error
    if (a < lowerBound) 
      -1
    else if (a >= lowerBound && b <= upperBound) 
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
  def -(p:Point3) = new Vector3(x - p.x, y - p.y, z - p.z)
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

class Intersection(val p:Point3, val normal:Vector3) {
  override def toString = String.format("p = %s, n = %s", p, normal)
}

trait Material {
  val c:Color
}

class GenericMaterial(val c:Color) extends Material {

}

// Surfaces: move to another file?

trait Surface {
  val material:Material
  def normal(p:Point3) = Vector3.Zero // default implementation
  def intersection(origin:Point3, d:Vector3):List[Intersection] = Nil // default implementation
}

class Sphere(center:Point3, radius:Double, val material:Material) extends Surface {
  val c = center
  val r = radius
  
  override def intersection(o:Point3, d:Vector3) = {
    // (o + td - c) dot (o + td - c) - R^2
    // (d dot d)t^2 + 2d dot (e - c)t + (e - c) dot (e - c) - R^2 = 0
    // Solve using quadratic equation

    val determinant = pow(d dot (o - c), 2) - (d dot d) * (((o - c) dot (o - c)) - (r * r))
    // Compare using error interval (for floating point inaccuracies)
    val comparison = Constants.floatCompare(determinant, 0)
    // Find the "time" at which the ray intersects the surface 
    val ts:List[Double] =
      if (comparison < 0) {
         // No solutions
         Nil
      } else {
        // a = -B term
        // b = A^2 term
        val a = -d dot (o - c)
        val b = d dot d
        if (comparison == 0) {
          // One solution
          (a / b) :: Nil
        } else {
          // Two solutions
          // c = B^2 - 4AC term
          val c = sqrt(determinant)
          ((a + c) / b) :: ((a - c) / b) :: Nil
        }
      }
    for (t <- ts) yield {
      val p = o + (d * t)
      new Intersection(p, p - o)
    }
  }

  override def toString = String.format("Sphere: center = %s, radius = %s", c, r.asInstanceOf[AnyRef])
}

class Triangle(val p1: Point3, val p2: Point3, val p3:Point3, val material:Material) extends Surface {

}

class Screen(val origin:Point3, val xAxis:Vector3, val yAxis:Vector3, 
    val w:Int, val h:Int) {
  def normal = (xAxis cross yAxis).direction

  def pixelAt(x:Double, y:Double) = 
    origin + xAxis * ((x + 0.5) / w)  + yAxis * ((y + 0.5) / h)

  override def toString = String.format("(%d x %d) screen: origin = %s, x = %s, y = %s", 
    w.asInstanceOf[AnyRef], h.asInstanceOf[AnyRef], origin, xAxis, yAxis)
}


