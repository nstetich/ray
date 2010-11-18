import scala.math._

trait Surface {
  val material:Material
  def intersection(origin:Point3, direction:Vector3):Option[Intersection]
}

class Sphere(center:Point3, radius:Double, val material:Material) extends Surface {
  val c = center
  val r = radius
  
  override def intersection(origin:Point3, direction:Vector3):Option[Intersection] = {
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

  override def toString = String.format("Sphere: center = %s, radius = %s, material = %s", c, r.asInstanceOf[AnyRef], material)
}

class Triangle(
  val v1: Vertex, 
  val v2: Vertex,  
  val v3: Vertex, 
  val material:Material
) extends Surface {

  override def intersection(origin: Point3, vd: Vector3) : Option[Intersection] = {
    val va = v1.location
    val vb = v2.location
    val vc = v3.location
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

    val t = -(f*D + e*E + d*F) / M
//    if (t < 0) return None

    val gamma = (i*D + h*E + g*F) / M
    if (gamma < 0 || gamma > 1) return None

    val beta = (j*A + k*B + l*C) / M
    if (beta < 0 || beta > 1 - gamma) return None

    val normal = v2.normal
 //   println(String.format("v = %s, t = %s, beta = %s, gamma = %s", vd, t.asInstanceOf[AnyRef], beta.asInstanceOf[AnyRef], gamma.asInstanceOf[AnyRef]))

    return Some(new Intersection(this.asInstanceOf[Surface], origin + (vd * t), normal, vd.magnitude * t, t))
  }

}

