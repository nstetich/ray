import java.io.File

val eye = P(0, 0, 10)
val screen = new Screen(P(-1.5, -1.5, 2), V(3, 0, 0), V(0, 3, 0), 400, 400)
val surfaces = new Sphere(P(0, 0, 0), 1.0, new GenericMaterial(Color.Yellow)) :: Nil
val lights = new PointLight(P(-10, 10, 10), Color.Red) :: new PointLight(P(10, 10, 10), Color.Green) :: Nil
val bgColor = Color.Black
val ambientLight = new Color(0.1, 0.1, 0.1)
val model = new Model(eye, screen, lights, surfaces, ambientLight, bgColor)
val file = new File("foo.png")
try {
  println("Rendering...")
  model.render(new File("foo.png"))
} catch {
   case e => e.printStackTrace()
}

