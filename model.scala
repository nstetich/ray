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

class Model(
  val eye:Point3,
  val screen:Screen,
  val lights:List[Light],
  val surfaces:List[Surface],
  val ambientLight:Color,
  val backgroundColor:Color)
{

  def lambert(intensity:Color, reflectance:Color, 
              normal:Vector3, light:Vector3) = {
    // c_i * c_r * cos(phi) -- Use dot product to determine cosine:
    // a dot b = mag(a) * mag(b) * cos(phi)
    intensity * reflectance * max(0, 
      (normal dot light) / (normal.magnitude * light.magnitude))
  }

  def phong(intensity:Color, reflectance:Color,
            eye:Vector3, normal:Vector3, light:Vector3) = {
    // Find the (unit) vector pointed halfway between the eye ray and light ray    
    val h = (eye + light).direction
    val n = normal.direction
    reflectance * intensity * pow((h dot n), 64)
    
  }

  def render(file:File) = {
    // Set up log file
    import java.io._
    val log = new PrintWriter(new BufferedWriter(new FileWriter("log.txt")))
    val imgBuf = new ImageBuffer(screen.w, screen.h)
    for ( x <- 0 until screen.w; y <- 0 until screen.h ) {
    val ray = (screen.pixelAt(x, y) - eye).direction
    // Gather all intersections with all objects, and pick the closest. 
    val intersections = surfaces.flatMap(_.intersection(eye, ray))
    if (!intersections.isEmpty) {
        val i = intersections.min // Pick the closest intersection
//        log.println(String.format("(%d,%d): P = %s, V = %s, t = %f", x.asInstanceOf[AnyRef], y.asInstanceOf[AnyRef], i.location, i.normal, i.t.asInstanceOf[AnyRef]))
        val colors = for (light <- lights) yield {
          val lightRay = light vectorFrom i.location
          (lambert(i.surface.material.reflectivity, light.color, i.normal, lightRay) +
           phong(i.surface.material.highlight, light.color, ray, i.normal, lightRay))
        }
        // y coordinates are numbered top to bottom for ImageBuffer
        val defaultColor = ambientLight * i.surface.material.reflectivity
        imgBuf.setPixel(x, screen.h - y, colors.foldLeft(defaultColor)(_ + _))
      } else {
 //       log.println(String.format("(%s,%s): No intersections", x.asInstanceOf[AnyRef], y.asInstanceOf[AnyRef]))
        imgBuf.setPixel(x, y, backgroundColor)
      }
    }
    log.close()
    imgBuf.writeToFile(file)
  }
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

object RenderTest {
  def main(args:Array[String]) = {
    val eye = P(0, 0, 8)
    val screen = new Screen(P(-1.5, -1.5, 0), V(3, 0, 0), V(0, 3, 0), 400, 400)
    val surfaces = new Sphere(P(0, 0, 0), 1.0, new GenericMaterial(Color.Red, Color.White)) :: Nil
    val lights = new PointLight(P(-10, 10, 10), Color.White) :: Nil
    val ambient = new Color(0.1, 0.1, 0.1)
    val bg = Color.Black
    val model = new Model(eye, screen, lights, surfaces, ambient, bg)
    val file = new File("foo.png")
    try {
      println("Rendering...")
      model.render(new File("foo.png"))
    } catch {
       case e => e.printStackTrace()
    }
  }
}

