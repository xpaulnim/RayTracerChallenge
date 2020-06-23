package raytracer

case class Ray(origin: Tuple, /* Point */
               direction: Tuple /* Vector */) {
  assert(origin.isPoint)
  assert(direction.isVector)

  def position(t: Double): Tuple = {
    // t is time. the direction vector is speed
    // speed = distance * time
    origin + direction * t
  }

}
