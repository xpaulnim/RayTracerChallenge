package raytracer

case class Intersection(t: Double, obj: SceneObj)

object Intersection {
  def hit(intersections: Seq[Intersection]): Option[Intersection] = {
    intersections
      .filter(_.t >= 0)
      .sortWith(_.t < _.t)
      .headOption
  }
}
