import scala.math._
import java.io.File

class Camera (val eye: Point3, val screen: Screen)

object Camera {
  def apply(eye: Point3, screen: Screen) = new Camera(eye, screen)
  def apply(location: Point3, viewAngle: Double, view: Vector3, 
      up: Vector3, xRes: Int, yRes: Int) = {
    require(viewAngle > 0 && viewAngle < 180 && xRes > 0 && yRes > 0)
    val eye = location
    val screen = {
      val ratio = yRes.asInstanceOf[Double] / xRes.asInstanceOf[Double]
      val width = view.magnitude * tan(0.5 * viewAngle.toRadians)
      val height = ratio * width
      val horiz = (view cross up) direction
      val vert = -(view cross horiz) direction
      val origin = eye + (view - (vert * (0.5 * height)) - (horiz * (0.5 * width)))
      val xAxis = horiz * width
      val yAxis = vert * height
      new Screen(origin, xAxis, yAxis, xRes, yRes)
    }
    new Camera(eye, screen) 
  }
}

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
  val camera: Camera,
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
    val n = normal
    val C = h dot n
    reflectance * intensity * (if (C > 0) pow(C, 64) else 0)
  }
  
  def colorAt(ray: Ray, t0: Double, t1: Double): Color = 
    colorAt(ray, t0, t1, 0, recursionDepth)
  
  def colorAt(ray: Ray, t0:Double, t1: Double, depth: Int, maxDepth: Int): Color = {
    // Evaluate all light sources
    val intersections = surfaces.flatMap(_.intersection(ray, t0, t1))
    if (!intersections.isEmpty) {
      val intersection = intersections.min
      val point = ray.origin + ray.vector * intersection.t
      val normal = {
        val Some(n) = intersection.surface.normal(point)
        n.direction
      }
//      val Some(normal) = intersection.surface.normal(point)
//      val normal:Vector3 = intersection.surface.normal(point) match {
//        case Some(n:Vector3) => n
//        case None => new Vector3(0, 0, 0)
//      }
//      println(String.format("colorAt(depth=%d, ray=%s): Surface = %s, Point = %s Normal = %s", depth.asInstanceOf[AnyRef], ray, intersection.surface, point, normal))
      val otherSurfaces = surfaces.filterNot(_ == intersection.surface)
      val directLightColors = 
        for ( light <- lights ) yield {
          val lightRay = new Ray(point, light vectorFrom point)
          val shadowIntersections = otherSurfaces.flatMap(_.intersection(lightRay, 0, 1))
          // Start the ray out far enough we don't return the same surface we are rendering.
//          val shadowIntersections = surfaces.flatMap(_.intersection(lightRay, Constants.Error, 1))
          if (shadowIntersections.isEmpty) 
            Some(
              lambert(intersection.surface.material.reflectance, 
                  light.color, normal, lightRay.vector) +
              phong(intersection.surface.material.highlight, light.color, 
                  ray.vector, normal, lightRay.vector)
            )
          else None
        }
      val defaultColor = ambientLight * intersection.surface.material.reflectance
      val reflectivity = intersection.surface.material.reflectivity
      val reflectedLightColor: Option[Color] = 
        if (reflectivity != Color.Black && depth < maxDepth) {
          val reflectionRay = new Ray(point, ray.vector reflectionAbout normal)
          Some(             
            // Start the ray out far enough we don't return the same surface we are rendering.
            reflectivity * colorAt(reflectionRay, Constants.Error + Constants.Error,
            scala.Double.PositiveInfinity, depth + 1, maxDepth)
          )
        } else None
      (reflectedLightColor :: directLightColors).flatten.foldLeft(defaultColor)(_ + _)
    } else backgroundColor
  }

  def render(file:File) = {
    val imgBuf = new ImageBuffer(camera.screen.w, camera.screen.h)
    for ( x <- 0 until camera.screen.w; y <- 0 until camera.screen.h ) {
      val ray = new Ray(camera.eye, (camera.screen.pixelAt(x, y) - camera.eye).direction)
//      println(String.format("Painting pixel at (%d, %d).", x.asInstanceOf[AnyRef], y.asInstanceOf[AnyRef]))
      val pixelColor = colorAt(ray, 0, scala.Double.PositiveInfinity)

      // y coordinates are numbered top to bottom for ImageBuffer but 
      // are numbered increasing bottom to top in this coordinate space
      val (pX, pY) = (x, camera.screen.h - y - 1)
      try {
        imgBuf.setPixel(pX, pY, pixelColor)
      } catch {
        case e:Exception => println( String.format("Error writing pixel at (%d, %d): %s", 
          pX.asInstanceOf[AnyRef], pY.asInstanceOf[AnyRef], e.getMessage()))
      } 
    }
    imgBuf.writeToFile(file)
  }

  override def toString = String.format("Model: eye = %s, screen = %s, surfaces = %s, lights = %s, ambient = %s, bg = %s", camera.eye, camera.screen, surfaces, lights, ambientLight, backgroundColor)

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


