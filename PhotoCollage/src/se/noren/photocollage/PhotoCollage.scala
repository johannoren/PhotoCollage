package se.noren.photocollage
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.io.File
import org.imgscalr.Scalr
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map

/**
 * Create photo collages out of a set of images.
 * Input:  - A motive that acts as blueprint for what you want to make the collage look like.
 *           Put the image in the directory "photos" and name it "motive.jpg".
 *         - A set of images to be used in the collage. Try to find images with various 
 *           medium brightness. Since brightness tend to even out in general, specifically 
 *           look for very dark and very bright pictures.
 *           
 * Output: - The collage will be created in the folder "photos" and be named "canvas.png".
 *           In addition to this a set temporary files will be created in the folder 
 *           "photos/inputphotots/tmp". These take time to generate and will be reused if 
 *           you try to generate more collages so don't delete them until you're finished
 *           for performance reasons.
 *           
 * Tips and tricks:
 *           When trying to match images into the collage the algorithm will search for 
 *           images with corresponding brightness to the area. At the end of the programs
 *           run along with the produced image, a lot of statistics about your image set
 *           is printed on standard out. Analyse that to figure out what brightness ranges 
 *           are lacking images.
 */
object PhotoCollage {

  /*
   * Change these constants to your preferences.
   */
  val CanvasImgPath = "photos\\canvas.png" 
  val MotiveImgPath = "photos\\motive.jpg" 
  val MotiveImgGreyPath = "photos\\motive_grey.jpg"
  val GreyImgFileSuffix = "_grey.png"
  val GreyImgFileAltSuffix = "_grey_alt.png"
  val SampleDirectory = "photos\\inputphotos"
  val targetWidth = 8192 * 2
  val noOfPhotosPerRow = 100
  val convertedImgWidth = targetWidth / noOfPhotosPerRow
  val reusePhotos = true
  
  /**
   * Read image from file
   */
  def readImage(path: String): BufferedImage = ImageIO.read(new File(path))
  
  /**
   * Write an image to file as PNG
   */
  def writeImage(path: String, image: BufferedImage) = ImageIO.write(image, "png", new File(path))
  
  /**
   * Does a certain file exist on disk
   */
  def fileExists(path: String): Boolean = new File(path).exists()
  
  /**
   * Resize image to have it longest side <= size. 
   * Proportions are not changed.
   */
  def resizeImage(img: BufferedImage, size: Int): BufferedImage = Scalr.resize(img, size)
  
  /**
   * Crop an image
   */
  def cropImage(img: BufferedImage, startx: Int, starty: Int, width: Int, height: Int): BufferedImage = Scalr.crop(img, startx, starty, width, height) 
    
  /**
   * Create a new image to draw to
   */
  def createNewImage(x: Int, y: Int) = new BufferedImage(x, y, BufferedImage.TYPE_BYTE_GRAY)
  
  /**
   * Paint an image on another image
   */
  def drawImageOnImage(canvas: BufferedImage, img: BufferedImage, x: Int, y: Int) {
    val g = canvas.getGraphics()
    g.drawImage(img, x, y, null)
    g.dispose()
  }
  
  /**
   * Convert an RGB value to greyscale
   */
  def rgb2gray(rgb: Int): Int = {
    val red   = (rgb >> 16) & 0xff;
    val green = (rgb >> 8) & 0xff;
    val blue  = (rgb) & 0xff;
    (red + green + blue) / 3
  }
  
  /**
   * Calculate the average brightness of an image
   */
  def brightness(img: BufferedImage): Int = brightness(img, 0, 0, img.getWidth(), img.getHeight())
  
  /**
   * Calculate the average brightness of a portion of an image.
   *  
   * @param img Image to analyse for average brightness.
   * @param startx Start x coordinate of image subset to analyze  
   * @param starty Start y coordinate of image subset to analyze 
   * @param stopx Stop x coordinate of image subset to analyze 
   * @param stopy Stop y coordinate of image subset to analyze
   * @return Average brightness of the subset of the image
   */
  def brightness(img: BufferedImage, startx: Int, starty: Int, stopx: Int, stopy: Int): Int = {

    @tailrec
    def estimateBrightness(x: Int, y: Int, maxx: Int, maxy: Int, aggr: Int): Int = {
      if (y == maxy) 
        aggr
      else if (x == maxx) 
        estimateBrightness(startx, y + 1, maxx, maxy, aggr)
      else 
        estimateBrightness(x + 1, y, maxx, maxy, aggr + rgb2gray(img.getRGB(x, y)))
    }
    
    /*
     * Average the brightness of the number of evaluated pixels with the aggregate of
     * their calculated values.
     */
    estimateBrightness(startx, starty, stopx, stopy, 0) / ((stopx - startx) * (stopy - starty))
  }  
  
  /**
   * Convert an image to a greyscale copy
   */
  def greyscale(img: BufferedImage): BufferedImage = {
    val copy = new BufferedImage(img.getWidth(), img.getHeight(), BufferedImage.TYPE_BYTE_GRAY)
    val g = copy.getGraphics()
    g.drawImage(img, 0, 0, null)
    g.dispose()
    copy
  }
  
  
  def preparePhotoCollection(directory: String): List[PhotoMetaData] = {
    
   def createRecord(img: BufferedImage, imgSize: Int, originalPath: String, newPath: String): PhotoMetaData = {
     val newImage = resizeImage(greyscale(img), imgSize)
     writeImage(newPath, newImage)
     println("Converted and wrote " + newPath)
     new PhotoMetaData(originalPath, newPath, brightness(newImage))
   }
    
    val list = ListBuffer[PhotoMetaData]()
    for (file <- new File(directory).list()) {
      if (file.contains(".") && !(file.contains(GreyImgFileSuffix) || file.contains(GreyImgFileAltSuffix))) {
	      val greyFileName = directory + "\\tmp\\" + file.substring(0, file.lastIndexOf(".")) + GreyImgFileSuffix
	      val greyFileNameAlt = directory + "\\tmp\\" + file.substring(0, file.lastIndexOf(".")) + GreyImgFileAltSuffix
          if (!fileExists(greyFileName)) {
	        val image = readImage(directory + "\\" + file)
	        if (image.getHeight() == image.getWidth())
	          list += createRecord(image, convertedImgWidth, file, greyFileName)
	        else {
	          // Not same proportions, create two images for this.
	          val dim = image.getHeight() min image.getWidth()
              list += createRecord(cropImage(image, 0, 0, dim, dim), convertedImgWidth, file, greyFileName)  
	          list += (
	            if (image.getHeight() < image.getWidth()) 
	              createRecord(cropImage(image, image.getWidth() - dim, 0, dim, dim), convertedImgWidth, file, greyFileNameAlt)  
	            else 
	              createRecord(cropImage(image, 0, image.getHeight() - dim, dim, dim), convertedImgWidth, file, greyFileNameAlt))  	            
	        }
	      } else {
    	    list += new PhotoMetaData(file, greyFileName, brightness(readImage(greyFileName)))
	      }
      }
    } 
    list.toList
  }
  
  def tileUsedNearby(usedTiles: Map[(Int, Int), PhotoMetaData], searchRadius: Int, x: Int, y: Int, tile: PhotoMetaData): Boolean = {
    for (i <- x - searchRadius to x + searchRadius)
      for (j <- y - searchRadius to y + searchRadius) {
        if (usedTiles.contains((i, j)) && usedTiles((i, j)) == tile)
          return true
      }
    false
  }
  
  def bestMatchingRecord(list: List[PhotoMetaData], brightness: Int, usedTiles: Map[(Int, Int), PhotoMetaData], searchRadius: Int, x: Int, y: Int): PhotoMetaData = {
    @tailrec
    def findBest(list: List[PhotoMetaData], best: PhotoMetaData): PhotoMetaData = {
      if (list.length == 0) 	// No more tiles to check
        best
      else if (tileUsedNearby(usedTiles, searchRadius, x, y, list.head)) // To close to someplace already used
        findBest(list.tail, best)
      else     // Check if it's a best match so far?
        findBest(list.tail, 
          if ((best.avgBrightness - brightness).abs < (list.head.avgBrightness - brightness).abs) best
          else list.head
        )
    }    
    findBest(list.tail, list.head)
  }
  
  def printCollectionStats(list: List[PhotoMetaData]) {
    val tuples = (0 until 256) zip (List.fill(256)(0))    // Create tuples like (0,0)(1,0)...
    val map = collection.mutable.Map(tuples: _*)
    
    for (ph <- list) {
      assert(ph.avgBrightness >= 0 && ph.avgBrightness < 256, "Brightness can't be " + ph.avgBrightness)
      map(ph.avgBrightness) += 1
    }
    
    println("----------------------------------")
    println("Collection contains " + list.length + " images.")
    for ((brightness, count) <- map.toList.sorted)
      println("brightness: " + brightness + " -> " + count + " photos.")
  }
  

    
  def main(args: Array[String]): Unit = {
    
    val motiveImg = readImage(MotiveImgPath)
    val motive = greyscale(motiveImg)
    writeImage(MotiveImgGreyPath, motive)
    
    val photos = preparePhotoCollection(SampleDirectory)
    val tileSizeTarget = targetWidth / noOfPhotosPerRow
    val targetHeight = targetWidth * motive.getHeight() / motive.getWidth()
    val noOfPhotosPerCol = targetHeight / tileSizeTarget
    val tileSizeMotive = motive.getWidth() / noOfPhotosPerRow
    
    val canvas = createNewImage(targetWidth, targetHeight)
    val usedTiles = Map[(Int, Int), PhotoMetaData]()
   
    println("Creating new image. using " + photos.length + " photos")
    var hits = 0
    var bigMiss= 0
    var remainingPhotos = photos
    for (x <- 0 until noOfPhotosPerRow)
      for (y <- 0 until noOfPhotosPerCol) {
        val avgBright = brightness(motive, x * tileSizeMotive, y * tileSizeMotive, (x + 1) * tileSizeMotive, (y + 1) * tileSizeMotive)
        val bestRecord = bestMatchingRecord(remainingPhotos, avgBright, usedTiles, 4, x, y)
        if (!reusePhotos)
          remainingPhotos = remainingPhotos.remove(b => b == bestRecord)
        println("Searching brightness " + avgBright + " \t found " + bestRecord.avgBrightness)
        hits += (if (avgBright == bestRecord.avgBrightness)  1 else 0)
        bigMiss += (if ((avgBright - bestRecord.avgBrightness).abs > 5) {
          println("Big miss: " + avgBright + " found " + bestRecord.avgBrightness)
          1 
        } else 0)
        drawImageOnImage(canvas, readImage(bestRecord.greyscalePath), x * tileSizeTarget, y * tileSizeTarget)
        
        // Store usage of tile
        usedTiles += ((x, y) -> bestRecord)
      }
    
    writeImage(CanvasImgPath, canvas)
    
    printCollectionStats(photos)
    
    println("In total matched " + noOfPhotosPerRow * noOfPhotosPerCol + " cells. Hits: " + hits + "\t Big misses: " + bigMiss)

  }

}