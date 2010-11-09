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

class Light(location:Point3, intensity:Color)

class Model(
  val eye:Point3,
  val screen:Screen,
  val lights:List[Light],
  val surfaces:List[Surface]) 
{
  def lambert(intensity:Color, reflectance:Color, 
              normal:Vector3, view:Vector3) = {
    intensity * reflectance * max(0, normal.direction dot view.direction)
  }

  def render(file:File) = {
    val imgBuf = new ImageBuffer(screen.w, screen.h)
    for ( x <- 0 until screen.w; y <- 0 until screen.h ) {
      val intersections = surfaces.flatMap( (surface) => {
        val ray = screen.pixelAt(x, y) - eye
        surface.intersections(eye, ray)
      })
      if (!intersections.isEmpty) {
        val i = intersections.max
        imgBuf.setPixel(x, y, new Color(1, 1, 1))
      } else 
        imgBuf.setPixel(x, y, new Color(0, 0, 0))
    }
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

object RenderTest {
  def main(args:Array[String]) = {
    val eye = new Point3(0, 0, 8)
    val screen = new Screen(new Point3(-1.5, -1.5, 0), new Vector3(3, 0, 0), new Vector3(0, 3, 0), 400, 400)
    val surfaces = new Sphere(new Point3(0, 0, 0), 1.0, new GenericMaterial(new Color(1, 0, 0))) :: Nil
    val lights = new Light(new Point3(-10, 10, 0), new Color(1, 1, 1)) :: Nil
    val model = new Model(eye, screen, lights, surfaces)
    val file = new File("foo.png")
    try {
      println("Rendering...")
      model.render(new File("foo.png"))
    } catch {
       case e => e.printStackTrace()
    }
  }
}

