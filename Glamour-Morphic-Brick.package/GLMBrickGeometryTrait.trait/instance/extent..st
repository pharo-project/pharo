brick-geometry
extent: aPoint
	"actually works only if width or height are not custom"
	"updates layout"
	self changed.
	
	self width: aPoint x.
	self height: aPoint y.
	self wrappedBounds bounds: (self brickBounds origin extent: aPoint).
	
	self flag: 'do I need to layout morphs inside?'.
	self doLayout.
	
	self changed