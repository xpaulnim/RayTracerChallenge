package raytracer

case class Intersection(t: Double, obj: SceneObj)

object Intersection {
  def hit(intersections: Set[Intersection]): Option[Intersection] = {
    intersections
      .filter(_.t >= 0)
      .toSeq
      .sortWith(_.t < _.t)
      .headOption
  }
}
