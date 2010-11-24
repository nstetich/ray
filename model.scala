import scala.math._
import java.io.File

class Screen(val origin:Point3, val xAxis:Vector3, val yAxis:Vector3, 
    val w:Int, val h:Int) {
  def normal = (xAxis cross yAxis).direction

  def pixelAt(x:Double, y:Double):Point3 = 
    origin + xAxis * ((x + 0.5) / w)  + yAxis * ((y + 0.5) / h)

  override def toString = String.format("(%d x %d) screen: origin = %s, x = %s, y = %s", 
    w.asInstanceOf[AnyRef], h.asInstanceOf[AnyRef], origin, xAxis, yAxis)
}

trait Light {
  val color:Color
  def vectorFrom(destination:Point3):Vector3
}

class PointLight(val location:Point3, val color:Color) extends Light {
  override def vectorFrom(destination:Point3) = location - destination
  override def toString = String.format("Light at %s, %s", location, color)
}

class DirectionalLight(direction:Vector3, val color:Color) extends Light {
  override def vectorFrom(destination:Point3) = direction
}

class Model (
  val eye: Point3,
  val screen: Screen,
  val lights: List[Light],
  val surfaces: List[Surface],
  val ambientLight: Color,
  val backgroundColor: Color,
  val recursionDepth: Int
) {

  def lambert(intensity: Color, reflectance: Color, 
              normal: Vector3, light: Vector3) = {
    // c_i * c_r * cos(phi) -- Use dot product to determine cosine:
    // a dot b = mag(a) * mag(b) * cos(phi)
    intensity * reflectance * max(0, 
      (normal dot light) / (normal.magnitude * light.magnitude))
  }

  def phong(intensity: Color, reflectance: Color,
            eye: Vector3, normal: Vector3, light: Vector3) = {
    // Find the (unit) vector pointed halfway between the eye ray and light ray    
    val h = (eye + light).direction
    val n = normal.direction
    val C = h dot n
    reflectance * intensity * (if (C > 0) pow(C, 64) else 0)
  }
  
  def colorAt(ray: Ray, t0: Double, t1: Double): Color = 
    colorAt(ray, t0, t1, 0, recursionDepth, None)
  
  def colorAt(ray: Ray, t0:Double, t1: Double, depth: Int, maxDepth: Int, 
      ignoredSurface: Option[Surface]): Color = {
    // Evaluate all light sources
    val intersections = surfaces.flatMap(_.intersection(ray, t0, t1))
    if (!intersections.isEmpty) {
      val intersection = intersections.min
      val point = ray.origin + ray.vector * intersection.t
//      val Some(normal) = intersection.surface.normal(point)
      val normal:Vector3 = intersection.surface.normal(point) match {
        case Some(n:Vector3) => n
        case _ => new Vector3(0, 0, 0)
      }
      val otherSurfaces = surfaces.filterNot(_ == intersection.surface)
      val directLightColors = 
        for ( light <- lights ) yield {
          val lightRay = new Ray(point, light vectorFrom point)
          val shadowIntersections = otherSurfaces.flatMap(_.intersection(lightRay, 0, 1))
          if (shadowIntersections.isEmpty) 
            Some(
              lambert(intersection.surface.material.reflectance, light.color, normal, lightRay.vector) +
              phong(intersection.surface.material.highlight, light.color, ray.vector, normal, lightRay.vector)
            )
          else None
        }
      val defaultColor = ambientLight * intersection.surface.material.reflectance
      val reflectedLightColor: Option[Color] = 
        if (depth < maxDepth) {
          val reflectionRay = new Ray(point, 
            ray.vector + (normal * (2 * (ray.vector dot normal))))
          Some(
            intersection.surface.material.reflectivity * 
            colorAt(reflectionRay, 0, scala.Double.PositiveInfinity, depth + 1, maxDepth, 
              Some(intersection.surface))
          )
        } else None
      (reflectedLightColor :: directLightColors).flatten.foldLeft(defaultColor)(_ + _)
    } else backgroundColor
  }

  def render(file:File) = {
    val imgBuf = new ImageBuffer(screen.w, screen.h)
    for ( x <- 0 until screen.w; y <- 0 until screen.h ) {
      val ray = new Ray(eye, (screen.pixelAt(x, y) - eye).direction)
      val pixelColor = colorAt(ray, 0, scala.Double.PositiveInfinity)

      val (pX, pY) = (x, screen.h - y - 1)
      try {
        // y coordinates are numbered top to bottom for ImageBuffer but 
        // are numbered increasing bottom to top in this coordinate space
        imgBuf.setPixel(pX, pY, pixelColor)
      } catch {
        case e:Exception => println( String.format("Error writing pixel at (%d, %d): %s", 
          pX.asInstanceOf[AnyRef], pY.asInstanceOf[AnyRef], e.getMessage()))
      } 
    }
    imgBuf.writeToFile(file)
  }

  override def toString = String.format("Model: eye = %s, screen = %s, surfaces = %s, lights = %s, ambient = %s, bg = %s", eye, screen, surfaces, lights, ambientLight, backgroundColor)

}

class ImageBuffer(w:Int, h:Int) {
  import java.awt.image.BufferedImage
  import javax.imageio.ImageIO
  import javax.imageio.stream.FileImageOutputStream

  val buf = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)

  def setPixel(x:Int, y:Int, value:Color) = buf.setRGB(x, y, value.intValue)

  def writeToFile(f:File) = {
    val fios = new FileImageOutputStream(f)
    try {
      ImageIO.write(buf, "png", fios)
    } finally {
      if (fios != null) {
        fios.close()
      }
    } 
  }
}

object P {
  def apply(x:Double, y:Double, z:Double) = new Point3(x, y, z)
}

object V {
  def apply(x:Double, y:Double, z:Double) = new Vector3(x, y, z)
}


