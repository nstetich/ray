import scala.math._

trait Surface {
  val material:Material
  def normal(p:Point3) = Vector3.Zero // default implementation
  def intersections(origin:Point3, d:Vector3):List[Intersection] = Nil // default implementation
}

class Sphere(center:Point3, radius:Double, val material:Material) extends Surface {
  val c = center
  val r = radius
  
  override def intersections(origin:Point3, direction:Vector3) = {
    val o = origin
    val d = direction
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
          // c = sqrt(B^2 - 4AC) term
          val c = sqrt(determinant)
          ((a + c) / b) :: ((a - c) / b) :: Nil
        }
      }
    for (t <- ts) yield {
      val p = o + (d * t)
      new Intersection(this.asInstanceOf[Surface], p, p - center, (p - o).magnitude)
    }
  }

  override def toString = String.format("Sphere: center = %s, radius = %s", c, r.asInstanceOf[AnyRef])
}

class Triangle(val p1: Point3, val p2: Point3, val p3:Point3, val material:Material) extends Surface {

}

