import java.io.File

val eye = P(0, 0, 8)
val screen = new Screen(P(-1.5, -1.5, 0), V(3, 0, 0), V(0, 3, 0), 400, 400)
val surfaces = new Sphere(P(0, 0, 0), 1.0, new GenericMaterial(Color.Red)) :: Nil
val lights = new DirectionalLight(V(1, 1, 0), Color.White) :: Nil
val model = new Model(eye, screen, lights, surfaces)
val file = new File("foo.png")
try {
  println("Rendering...")
  model.render(new File("foo.png"))
} catch {
   case e => e.printStackTrace()
}

