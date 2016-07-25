transforming
translateBy: aPoint 
	"Translate the receiver's offset."

	self offset: (self offset translateBy: aPoint)