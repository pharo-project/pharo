menu
setRotationCenterFrom: aPoint
	"Polygons store their referencePosition."
	self setProperty: #referencePosition toValue: aPoint