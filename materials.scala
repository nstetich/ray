trait Material {
  val reflectance: Color
  val reflectivity: Color
  val highlight: Color
}

class GenericMaterial(
  val reflectance: Color,
  val reflectivity: Color,
  val highlight: Color
) extends Material {

}


