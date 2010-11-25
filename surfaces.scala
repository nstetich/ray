import scala.math._

trait Surface {
  val material:Material
  def intersection(ray:Ray, tmin:Double, tmax:Double):Option[Intersection]
  def normal(p:Point3):Option[Vector3]
}

class Sphere(center:Point3, radius:Double, val material:Material) extends Surface {
  val c = center
  val r = radius
  
  override def intersection(ray:Ray, tmin:Double, tmax:Double):Option[Intersection] = {
    val dst = ray.origin - this.c
    val b = dst dot (ray.vector.direction)
    val c = (dst dot dst) - r * r
    val d = b * b - c
    if (d > 0) {
      val t = -b - sqrt(d)
//      val p = ray.origin + (ray.vector * t)
//      val n = p - this.c
//      val dist = direction.magnitude * t
//      if (Constants.floatCompare(t, tmin) > 0 &&
//        Constants.floatCompare(t, tmax) < 0) {
      if (t > tmin && t < tmax) {  
        Some(new Intersection(this.asInstanceOf[Surface], t))
      } else {
        None
      }
    } else
      None
  }

  override def normal(p:Point3):Option[Vector3] = {
    val n = p - this.c
    if (Constants.floatCompare(n.magnitude, this.r) != 0) {
      println(String.format("n.magnitude = %f, radius = %f", n.magnitude.asInstanceOf[AnyRef], this.r.asInstanceOf[AnyRef]))
//    None
      Some(n)
    } else {
      Some(n)
    }
//    Some(p - c)
  }

  override def toString = String.format("Sphere: center = %s, radius = %s, material = %s", c, r.asInstanceOf[AnyRef], material)
}

class Triangle(
  val v1: Vertex, 
  val v2: Vertex,  
  val v3: Vertex, 
  val material:Material
) extends Surface {

  override def intersection(ray:Ray, tmin:Double, tmax:Double) : Option[Intersection] = {
    val va = v1.location
    val vb = v2.location
    val vc = v3.location
    val a = va.x - vb.x
    val b = va.y - vb.y
    val c = va.z - vb.z
    val d = va.x - vc.x
    val e = va.y - vc.y
    val f = va.z - vc.z
    val g = ray.vector.x
    val h = ray.vector.y
    val i = ray.vector.z
    val j = va.x - ray.origin.x
    val k = va.y - ray.origin.y
    val l = va.z - ray.origin.z
    val A = e*i - h*f
    val B = g*f - d*i
    val C = d*h - e*g
    val D = a*k - j*b
    val E = j*c - a*l
    val F = b*l - k*c
    val M = a*A + b*B + c*C

    val t = -(f*D + e*E + d*F) / M
    if (Constants.floatCompare(t, tmin) < 0 || 
      Constants.floatCompare(t, tmax) > 0) return None
//    if (t < tmin || t > tmax) return None

    val gamma = (i*D + h*E + g*F) / M
    if (Constants.floatCompare(gamma, 0) < 0 ||
      Constants.floatCompare(gamma, 1) > 0) return None
//    if (gamma < 0 || gamma > 1) return None

    val beta = (j*A + k*B + l*C) / M
    if (Constants.floatCompare(beta, 0) < 0 || 
      Constants.floatCompare(beta, 1 - gamma) > 0) return None
//    if (beta < 0 || beta > 1 - gamma) return None

    return Some(new Intersection(this.asInstanceOf[Surface], t))
  }

  override def normal(p:Point3):Option[Vector3] = {
    // TODO: Barycentric interpolation of stored normals
    return Some(v1.normal)
  }

}
