trait Material {
  val reflectivity:Color
  val highlight:Color
}

class GenericMaterial(
  val reflectivity:Color,
  val highlight:Color
) extends Material {

}


