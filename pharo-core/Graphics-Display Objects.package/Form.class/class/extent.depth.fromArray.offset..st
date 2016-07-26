instance creation
extent: extentPoint depth: bitsPerPixel fromArray: anArray offset: offsetPoint 
	"Answer an instance of me with a pixmap of the given depth initialized from anArray."

	^ (self extent: extentPoint depth: bitsPerPixel)
		offset: offsetPoint;
		initFromArray: anArray
