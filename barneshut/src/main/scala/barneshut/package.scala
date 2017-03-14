import common._
import barneshut.conctrees._

package object barneshut {

  class Boundaries {
    var minX = Float.MaxValue

    var minY = Float.MaxValue

    var maxX = Float.MinValue

    var maxY = Float.MinValue

    def width = maxX - minX

    def height = maxY - minY

    def size = math.max(width, height)

    def centerX = minX + width / 2

    def centerY = minY + height / 2

    override def toString = s"Boundaries($minX, $minY, $maxX, $maxY)"
  }

  object Quad {

    def massXY(qs: Seq[Quad]): (Float, Float) = {
      qs.foldLeft((0f, 0f))((acc, q) => (acc._1 + q.mass * q.massX, acc._2 + q.mass * q.massY))
    }

    def summarizeBodies(qs: Seq[Body]): (Float, Float, Float) = {
      val sums = qs.foldLeft((0f, 0f, 0f))((acc, b) => (acc._1 + b.mass, acc._2 + b.mass * b.x, acc._3 + b.mass * b.y))
      (sums._1, sums._2 / sums._1, sums._3 / sums._1)
    }
  }

  sealed abstract class Quad {
    def massX: Float

    def massY: Float

    def mass: Float

    def centerX: Float

    def centerY: Float

    def size: Float

    def total: Int

    def insert(b: Body): Quad

    def contains(b: Body): Boolean = {
      val halfSize = size / 2
      (centerX - halfSize <= b.x) && (b.x <= centerX + halfSize) && (centerY - halfSize <= b.y) && (b.y <= centerY + halfSize)
    }

  }

  case class Empty(centerX: Float, centerY: Float, size: Float) extends Quad {
    def massX: Float = centerX
    def massY: Float = centerY
    def mass: Float = 0
    def total: Int = 0
    def insert(b: Body): Quad = Leaf(centerX, centerY, size, Seq(b))
  }

  case class Fork(
    nw: Quad, ne: Quad, sw: Quad, se: Quad
  ) extends Quad {
    val centerX: Float = nw.centerX + nw.size / 2
    val centerY: Float = nw.centerY + nw.size / 2
    val size: Float = nw.size * 2
    val quads = List(nw, ne, sw, se)
    val mass: Float = quads.foldLeft(0f)(_ + _.mass)
    val massX = if (mass == 0f) centerX else quads.foldLeft(0f)((acc, q) => acc + q.mass * q.massX) / mass
    val massY = if (mass == 0f) centerY else quads.foldLeft(0f)((acc, q) => acc + q.mass * q.massY) / mass
    val total: Int = quads.foldLeft(0)(_ + _.total)

    def insert(b: Body): Fork = {
      val q = quads.find(_.contains(b)).get
      val newq = q.insert(b)
      if (q eq nw) {
        Fork(newq, ne, sw, se)
      } else if (q eq ne) {
        Fork(nw, newq, sw, se)
      } else if (q eq sw) {
        Fork(nw, ne, newq, se)
      } else {
        Fork(nw, ne, sw, newq)
      }
    }
  }

  case class Leaf(centerX: Float, centerY: Float, size: Float, bodies: Seq[Body])
  extends Quad {
    val (mass, massX, massY) = Quad.summarizeBodies(bodies)
    val total: Int = bodies.length
    def insert(b: Body): Quad = {
      if (size > minimumSize) {
        val quarter = size / 4
        val half = size / 2
        val nw = Empty(centerX - quarter, centerY - quarter, half)
        val ne = Empty(centerX + quarter, centerY - quarter, half)
        val sw = Empty(centerX - quarter, centerY + quarter, half)
        val se = Empty(centerX + quarter, centerY + quarter, half)
        val emptyFork = Fork(nw, ne, sw, se)
        (bodies :+ b).foldLeft(emptyFork)(_.insert(_))
      } else {
        Leaf(centerX, centerY, size, bodies :+ b)
      }
    }
  }

  def minimumSize = 0.00001f

  def gee: Float = 100.0f

  def delta: Float = 0.01f

  def theta = 0.5f

  def eliminationThreshold = 0.5f

  def force(m1: Float, m2: Float, dist: Float): Float = gee * m1 * m2 / (dist * dist)

  def distance(x0: Float, y0: Float, x1: Float, y1: Float): Float = {
    math.sqrt((x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0)).toFloat
  }

  class Body(val mass: Float, val x: Float, val y: Float, val xspeed: Float, val yspeed: Float) {

    def updated(quad: Quad): Body = {
      var netforcex = 0.0f
      var netforcey = 0.0f

      def addForce(thatMass: Float, thatMassX: Float, thatMassY: Float): Unit = {
        val dist = distance(thatMassX, thatMassY, x, y)
        /* If the distance is smaller than 1f, we enter the realm of close
         * body interactions. Since we do not model them in this simplistic
         * implementation, bodies at extreme proximities get a huge acceleration,
         * and are catapulted from each other's gravitational pull at extreme
         * velocities (something like this:
         * http://en.wikipedia.org/wiki/Interplanetary_spaceflight#Gravitational_slingshot).
         * To decrease the effect of this gravitational slingshot, as a very
         * simple approximation, we ignore gravity at extreme proximities.
         */
        if (dist > 1f) {
          val dforce = force(mass, thatMass, dist)
          val xn = (thatMassX - x) / dist
          val yn = (thatMassY - y) / dist
          val dforcex = dforce * xn
          val dforcey = dforce * yn
          netforcex += dforcex
          netforcey += dforcey
        }
      }

      def traverse(quad: Quad): Unit = (quad: Quad) match {
        case Empty(_, _, _) =>
          // no force
        case Leaf(_, _, _, bodies) =>
          bodies.foreach(b => addForce(b.mass, b.x, b.y))
        case Fork(nw, ne, sw, se) =>
          if (quad.size / distance(x, y, quad.massX, quad.massY) < theta) {
            addForce(quad.mass, quad.massX, quad.massY)
          } else {
            Seq(nw, ne, sw, se).foreach(q => traverse(q))
          }
      }

      traverse(quad)

      val nx = x + xspeed * delta
      val ny = y + yspeed * delta
      val nxspeed = xspeed + netforcex / mass * delta
      val nyspeed = yspeed + netforcey / mass * delta

      new Body(mass, nx, ny, nxspeed, nyspeed)
    }

    override def toString = s"Body($mass, $x, $y, $xspeed, $yspeed)"

  }

  val SECTOR_PRECISION = 8

  class SectorMatrix(val boundaries: Boundaries, val sectorPrecision: Int) {
    val sectorSize = boundaries.size / sectorPrecision
    val matrix = new Array[ConcBuffer[Body]](sectorPrecision * sectorPrecision)
    for (i <- 0 until matrix.length) matrix(i) = new ConcBuffer

    def +=(b: Body): SectorMatrix = {
      val bx = verify(b.x, boundaries.minX, boundaries.maxX)
      val by = verify(b.y, boundaries.minY, boundaries.maxY)
      val i: Int = ((bx - boundaries.minX) / sectorSize).toInt
      val j: Int = ((by - boundaries.minY) / sectorSize).toInt
      this(i, j) += b
      this
    }

    def apply(x: Int, y: Int) = matrix(y * sectorPrecision + x)

    def combine(that: SectorMatrix): SectorMatrix = {
      val result = new SectorMatrix(this.boundaries, this.sectorPrecision)
      this.matrix.zip(that.matrix).zipWithIndex.foreach { case ((cb1, cb2), i) => result.matrix(i) = cb1.combine(cb2)}
      result
    }

    def toQuad(parallelism: Int): Quad = {
      def BALANCING_FACTOR = 4
      def quad(x: Int, y: Int, span: Int, achievedParallelism: Int): Quad = {
        if (span == 1) {
          val sectorSize = boundaries.size / sectorPrecision
          val centerX = boundaries.minX + x * sectorSize + sectorSize / 2
          val centerY = boundaries.minY + y * sectorSize + sectorSize / 2
          var emptyQuad: Quad = Empty(centerX, centerY, sectorSize)
          val sectorBodies = this(x, y)
          sectorBodies.foldLeft(emptyQuad)(_ insert _)
        } else {
          val nspan = span / 2
          val nAchievedParallelism = achievedParallelism * 4
          val (nw, ne, sw, se) =
            if (parallelism > 1 && achievedParallelism < parallelism * BALANCING_FACTOR) parallel(
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            ) else (
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            )
          Fork(nw, ne, sw, se)
        }
      }

      quad(0, 0, sectorPrecision, 1)
    }

    private def verify(value: Float, min: Float, max: Float) =
      if (value < min) min
      else if (value > max) max
      else value

    override def toString = s"SectorMatrix(#bodies: ${matrix.map(_.size).sum})"
  }

  class TimeStatistics {
    private val timeMap = collection.mutable.Map[String, (Double, Int)]()

    def clear() = timeMap.clear()

    def timed[T](title: String)(body: =>T): T = {
      var res: T = null.asInstanceOf[T]
      val totalTime = /*measure*/ {
        val startTime = System.currentTimeMillis()
        res = body
        (System.currentTimeMillis() - startTime)
      }

      timeMap.get(title) match {
        case Some((total, num)) => timeMap(title) = (total + totalTime, num + 1)
        case None => timeMap(title) = (0.0, 0)
      }

      println(s"$title: ${totalTime} ms; avg: ${timeMap(title)._1 / timeMap(title)._2}")
      res
    }

    override def toString = {
      timeMap map {
        case (k, (total, num)) => k + ": " + (total / num * 100).toInt / 100.0 + " ms"
      } mkString("\n")
    }
  }
}
