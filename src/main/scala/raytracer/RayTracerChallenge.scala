package raytracer


case class Projectile(position: Tuple, velocity: Tuple)
case class Environment(gravity: Tuple, wind: Tuple)

object RayTracerChallenge extends App {
	val canvas = Canvas(900, 550)

	def tick(projectile: Projectile, environment: Environment) = {
		val position = projectile.position + projectile.velocity
		val velocity = projectile.velocity + environment.gravity + environment.wind

		Projectile(position, velocity)
	}

	var projectile = Projectile(Point(0, 1, 0), Vector(1, 1.8, 0).normalise() * 11.25)
	val environment = Environment(Vector(0, -0.1, 0), Vector(-0.01, 0, 0))

	for(i <- 0 until 600) {
		projectile = tick(projectile, environment)

		canvas.writePixel(projectile.position.x, projectile.position.y, Color(255, 0, 0))
	}

	Utils.writeStrToFile(canvas.toPPM())
}
