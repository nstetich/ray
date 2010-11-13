import scala.math._

trait Surface {
  val material:Material
  def normal(p:Point3) = Vector3.Zero // default implementation
  def intersections(origin:Point3, d:Vector3):List[Intersection] = Nil // default implementation
  def intersection(origin:Point3, direction:Vector3):Option[Intersection] = None
}

class Sphere(center:Point3, radius:Double, val material:Material) extends Surface {
  val c = center
  val r = radius
  
  override def intersection(origin:Point3, direction:Vector3):Option[Intersection] = {
//    float intersectRaySphere(const Ray &ray, const Sphere &sphere) {
//	    Vec dst = ray.o - sphere.o;
//	    float B = dot(dst, ray.d);
//	    float C = dot(dst, dst) - sphere.r2;
//	    float D = B*B - C;
//	    return D > 0 ? -B - sqrt(D) : std::numeric_limits<float>::infinity();
//    }
    val dst = origin - this.c
    val b = dst dot (direction.direction)
    val c = (dst dot dst) - r * r
    val d = b * b - c
    if (d > 0) {
      val t = -b - sqrt(d)
      val p = origin + (direction * t)
      val n = p - this.c
      val dist = direction.magnitude * t
      Some(new Intersection(this.asInstanceOf[Surface], p, n, dist, t))
    } else
      None
  }

  // Broken 
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
      new Intersection(this.asInstanceOf[Surface], p, p - center, (p - o).magnitude, t)
    }
  }

  override def toString = String.format("Sphere: center = %s, radius = %s, material = %s", c, r.asInstanceOf[AnyRef], material)
}

class Triangle(val p1: Point3, val p2: Point3, val p3:Point3, val material:Material) extends Surface {

}

