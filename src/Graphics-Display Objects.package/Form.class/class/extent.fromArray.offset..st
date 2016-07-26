instance creation
extent: extentPoint fromArray: anArray offset: offsetPoint 
	"Answer an instance of me of depth 1 with bitmap initialized from anArray."

	^ (self extent: extentPoint depth: 1)
		offset: offsetPoint;
		initFromArray: anArray
