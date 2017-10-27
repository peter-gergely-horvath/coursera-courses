import common._
import barneshut.conctrees._

import scala.annotation.tailrec

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

  sealed abstract class Quad {
    def massX: Float

    def massY: Float

    def mass: Float

    def centerX: Float

    def centerY: Float

    def size: Float

    def total: Int

    def insert(b: Body): Quad
  }

  case class Empty(centerX: Float, centerY: Float, size: Float) extends Quad {
    def massX: Float = centerX
    def massY: Float = centerY
    def mass: Float = 0
    def total: Int = 0
    def insert(b: Body): Quad = Leaf(centerX, centerY, size, List(b))
  }

  case class Fork(
    nw: Quad, ne: Quad, sw: Quad, se: Quad
  ) extends Quad {
    val centerX: Float = (nw.centerX + ne.centerX) / 2
    val centerY: Float = (nw.centerY + sw.centerY) / 2
    val size: Float = nw.size + sw.size
    val mass: Float = nw.mass + ne.mass + sw.mass + se.mass
    val massX: Float = {
      if (mass == 0) centerX
      else ((nw.massX * nw.mass) + (ne.massX * ne.mass) + (sw.massX * sw.mass) + (se.massX * se.mass)) / mass
    }
    val massY: Float = {
      if (mass == 0) centerY
      else ((nw.massY * nw.mass) + (ne.massY * ne.mass) + (sw.massY * sw.mass) + (se.massY * se.mass)) / mass
    }
    val total: Int = nw.total + ne.total + sw.total + se.total

    def insert(b: Body): Fork = {
      /*
      Note that we consider the top left corner to be at coordinate (0, 0).
      We also consider the x axis to grow to the right and the y axis to the bottom.
      */

      if (b.x < centerX && b.y < centerY) Fork(nw.insert(b), ne, sw, se)
      else if (b.x >= centerX && b.y < centerY) Fork(nw, ne.insert(b), sw, se)
      else if (b.x < centerX && b.y >= centerY) Fork(nw, ne, sw.insert(b), se)
      else if (b.x >= centerX && b.y >= centerY) Fork(nw, ne, sw, se.insert(b))
      else sys.error(s"Unexpected position: x:$b.x, y:$b.y")
    }
  }

  case class Leaf(centerX: Float, centerY: Float, size: Float, bodies: Seq[Body])
  extends Quad {
    val mass : Float = bodies.map(_.mass).sum
    val massX = bodies.map(it => it.x * it.mass).sum / mass
    val massY = bodies.map(it => it.y * it.mass).sum / mass

    val total: Int = bodies.size

    /**
      * If the size of a Leaf is greater than a predefined constant minimumSize,
      * inserting an additonal body into that Leaf quadtree creates a Fork quadtree
      * with empty children, and adds all the bodies into that Fork (including the new body).
      * Otherwise, inserting creates another Leaf with all the existing bodies and the new one.
      *
      * @param b the body to insert
      * @return the new Quad
      */
    def insert(b: Body): Quad = {

      val newBodies = b +: bodies

      if (size <= minimumSize) Leaf(centerX, centerY, size, newBodies)
      else {

        // we go for the center of the new Quad,
        // which is half of the halves, hence we divide by 4
        val positionOffset = size / 4

        val westCenterX = centerX - positionOffset
        val eastCenterX = centerX + positionOffset

        val northCenterY = centerY - positionOffset;
        val southCenterY = centerY + positionOffset;

        val halfSize = size / 2

        // north: Y is the same
        val newNorthWest = Empty(westCenterX, northCenterY, halfSize)
        val newNorthEast = Empty(eastCenterX, northCenterY, halfSize)

        // south: Y is the same
        val newSouthWest = Empty(westCenterX, southCenterY, halfSize)
        val newSouthEast = Empty(eastCenterX, southCenterY, halfSize)


        val theFork = Fork(newNorthWest, newNorthEast, newSouthWest, newSouthEast)

        newBodies.foreach(it => theFork.insert(it))

        theFork
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
          // no force:
          // no change, nothing to do
        case Leaf(_, _, _, bodies) =>
          // add force contribution of each body by calling addForce
          bodies.foreach(theBody => addForce(theBody.mass, theBody.x, theBody.y))
        case Fork(nw, ne, sw, se) =>
          // see if node is far enough from the body,
          // or recursion is needed
          val dist = distance(x, y, quad.massX, quad.massY)

          if (quad.size / dist < theta) {
            // a fork quadtree that is sufficiently far away acts as a single point of mass
            addForce(quad.mass, quad.massX, quad.massY)

          } else {
            // a fork quadtree that is not sufficiently far away must be recursively traversed
            traverse(nw)
            traverse(ne)
            traverse(sw)
            traverse(se)
          }
      }

      traverse(quad)

      val nx = x + xspeed * delta
      val ny = y + yspeed * delta
      val nxspeed = xspeed + netforcex / mass * delta
      val nyspeed = yspeed + netforcey / mass * delta

      new Body(mass, nx, ny, nxspeed, nyspeed)
    }

  }

  val SECTOR_PRECISION = 8

  class SectorMatrix(val boundaries: Boundaries, val sectorPrecision: Int) {
    val sectorSize = boundaries.size / sectorPrecision
    val matrix = new Array[ConcBuffer[Body]](sectorPrecision * sectorPrecision)
    for (i <- 0 until matrix.length) matrix(i) = new ConcBuffer

    def +=(b: Body): SectorMatrix = {

      def forceRange(value: Float, lowerBoundInclusive : Float, upperBoundExclusive : Float): Float = {
        if(value < lowerBoundInclusive) lowerBoundInclusive
        else if(value >= upperBoundExclusive) upperBoundExclusive-1
        else value
      }

      def rescale(value: Float, min: Float, max: Float): Float = {
        (value - min)/(max-min)
      }

      val restrictedX = forceRange(b.x, boundaries.minX, boundaries.maxX)
      val restrictedY = forceRange(b.y, boundaries.minY, boundaries.maxY)

      val rescaledX = rescale(restrictedX, boundaries.minX, boundaries.maxX)
      val rescaledY = rescale(restrictedY, boundaries.minY, boundaries.maxY)

      val actualX : Int = (rescaledX * sectorPrecision).toInt
      val actualY : Int = (rescaledY * sectorPrecision).toInt

      val selectedSector : ConcBuffer[Body] = this.apply(actualX, actualY)

      selectedSector += b

      this
    }

    def apply(x: Int, y: Int) = {
      val rowOffset = y * sectorPrecision
      matrix(rowOffset + x)
    }

    def combine(that: SectorMatrix): SectorMatrix = {
      val indexRange = 0.until(matrix.length)

      for (i <- indexRange) {
        val thisMatrix = matrix(i)
        val thatMatrix = that.matrix(i);

        matrix(i) = thisMatrix.combine(thatMatrix)
      }

      this
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
