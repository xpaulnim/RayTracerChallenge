package raytracer

case class Sphere() extends SceneObj {
  // TODO: mutable?
  var transform: Matrix = IdentityMatrix()

  def intersect(ray: Ray): Seq[Intersection] = {
    val tranformedRay = ray.transform(transform.inverse())

    // assume sphere is centered at world origin
    val sphereCenter = Point(0, 0, 0)
    val sphereToRay = tranformedRay.origin - sphereCenter

    val a = tranformedRay.direction.dot(tranformedRay.direction)
    val b = 2.0 * tranformedRay.direction.dot(sphereToRay)
    val c = sphereToRay.dot(sphereToRay) - 1.0

    // hello quadratic formula :D
    val discriminant = math.pow(b, 2.0) - 4.0 * a * c

    if (discriminant < 0) {
      Seq()
    } else {
      val t1 = (-b - math.sqrt(discriminant)) / (2.0 * a)
      val t2 = (-b + math.sqrt(discriminant)) / (2.0 * a)

      Seq(Intersection(t1, this), Intersection(t2, this)).sortWith(_.t < _.t)
    }
  }
}
