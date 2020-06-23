package raytracer

case class Sphere() extends SceneObj {
  def intersect(ray: Ray): Seq[Intersection] = {
    // assume sphere is centered at world origin
    val sphereCenter = Point(0, 0, 0)
    val sphereToRay = ray.origin - sphereCenter

    val a = ray.direction.dot(ray.direction)
    val b = 2 * ray.direction.dot(sphereToRay)
    val c = sphereToRay.dot(sphereToRay) - 1

    // hello quadratic formula :D
    val discriminant = math.pow(b, 2) - 4 * a * c

    if (discriminant < 0) {
      Seq()
    } else {
      val t1 = (-b - math.sqrt(discriminant)) / (2 * a)
      val t2 = (-b + math.sqrt(discriminant)) / (2 * a)

      Seq(Intersection(t1, this), Intersection(t2, this)).sortWith(_.t < _.t)
    }
  }
}
