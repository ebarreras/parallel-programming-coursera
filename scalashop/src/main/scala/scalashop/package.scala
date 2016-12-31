
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  def sum(c1: Tuple4[Int,Int,Int,Int], c2: Tuple4[Int,Int,Int,Int]) =
    (c1._1 + c2._1, c1._2 + c2._2, c1._3 + c2._3, c1._4 + c2._4)

  def div(c: Tuple4[Int,Int,Int,Int], n: Int) =
    (c._1/n, c._2/n, c._3/n, c._4/n)


  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    // TODO implement using while loops
    val box = for {
      i <- x - radius to x + radius if i >= 0 && i < src.width
      j <- y - radius to y + radius if j >= 0 && j < src.height
    } yield src.apply(i,j)
    val ave = div(box.map(c => (red(c), green(c), blue(c), alpha(c))).fold((0, 0, 0, 0))(sum), box.length)
    rgba(ave._1, ave._2, ave._3, ave._4)
  }

}
