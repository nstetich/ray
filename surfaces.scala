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

class Triangle(
  val v1: Vertex, 
  val v2: Vertex,  
  val v3: Vertex, 
  val material:Material
) extends Surface {

  override def intersection(origin: Point3, vd: Vector3) : Option[Intersection] = {
    val va = v2.location - v1.location
    val vb = v3.location - v2.location
    val vc = v3.location - v1.location
    val a = va.x - vb.x
    val b = va.y - vb.y
    val c = va.z - vb.z
    val d = va.x - vc.x
    val e = va.y - vc.y
    val f = va.z - vc.z
    val g = vd.x
    val h = vd.y
    val i = vd.z
    val j = va.x - origin.x
    val k = va.y - origin.y
    val l = va.z - origin.z
    val A = e*i - h*f
    val B = g*f - d*i
    val C = d*h - e*g
    val D = a*k - j*b
    val E = j*c - a*l
    val F = b*l - k*c
    val M = a*A + b*B + c*C
    val t = (f*D + e*E + d*F) / M
    val gamma = (i*D + h*E + g*F) / M
    if (gamma < 0 || gamma > 0) return None
    val beta = (j*A + k*B + l*C) / M
    if (beta < 0 || beta > 1 - gamma) return None
    val normal = v2.normal
    println(String.format("v = %s, t = %s, beta = %s, gamma = %s", vd, t.asInstanceOf[AnyRef], beta.asInstanceOf[AnyRef], gamma.asInstanceOf[AnyRef]))
    return Some(new Intersection(this.asInstanceOf[Surface], origin + (vd * t), normal, vd.magnitude * t, t))
  }

}

