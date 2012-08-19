package se.noren.photocollage

/**
 * Keeps track of an image source.
 */
class PhotoMetaData(val originalPath: String, 
					var greyscalePath: String, 
					var avgBrightness: Int) {

  override def toString() = {
    "Brightness: " + avgBrightness + " \tpath: " + originalPath + " \tgreyscale path: " + greyscalePath
  }
}