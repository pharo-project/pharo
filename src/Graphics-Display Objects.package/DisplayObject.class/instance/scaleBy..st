transforming
scaleBy: aPoint 
	"Scale the receiver's offset by aPoint."

	self offset: (self offset scaleBy: aPoint)