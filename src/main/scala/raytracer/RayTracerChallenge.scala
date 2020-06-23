package raytracer

import raytracer.MatrixTransformation.{rotationX, rotationY, rotationZ, translation, scaling}

import scala.math.Pi

case class Projectile(position: Tuple, velocity: Tuple)

case class Environment(gravity: Tuple, wind: Tuple)

object RayTracerChallenge extends App {

  val canvas = clock()

  Utils.writeStrToFile(canvas.toPPM())

  def clock(): Canvas = {
    val dim = 100
    val canvas = Canvas(dim, dim)
    val clockRadius = 45

    (1 to 12).map(h => {
      val hour = rotationY(h * Pi / 6) * Point(0, 0, 1)

      Point(hour.x * clockRadius, hour.y, hour.z * clockRadius) + Point(dim / 2, dim / 2, dim / 2)
    }).foreach(point => canvas.writePixel(point.x, point.z, Color(255, 0, 0)))

    canvas
  }

  def projectile(): Canvas = {
    val canvas = Canvas(900, 550)

    def tick(projectile: Projectile, environment: Environment) = {
      val position = projectile.position + projectile.velocity
      val velocity = projectile.velocity + environment.gravity + environment.wind

      Projectile(position, velocity)
    }

    var projectile = Projectile(Point(0, 1, 0), Vector(1, 1.8, 0).normalise() * 11.25)
    val environment = Environment(Vector(0, -0.1, 0), Vector(-0.01, 0, 0))

    for (_ <- 0 until 600) {
      projectile = tick(projectile, environment)

      canvas.writePixel(projectile.position.x, projectile.position.y, Color(255, 0, 0))
    }

    canvas
  }
}
