instance creation
extent: extentPoint offset: offsetPoint 
	"Answer an instance of me with a blank bitmap of depth 1."

	^ (self extent: extentPoint depth: 1) offset: offsetPoint
