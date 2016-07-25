geometry
extent: aPoint
	"Do it normally"
	
	self changed.
	bounds := bounds topLeft extent: aPoint.
	self layoutChanged.
	self changed.
