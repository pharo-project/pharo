scrollpane-testing
isPartiallyVisible: aBrick
	|myBounds testingBounds|
	"need to normalize bounds to take only y coordinate into account"
	
	myBounds := (self allowedBounds withWidth: 1) translateBy: (self allowedBounds origin negated x@0).
	testingBounds :=  (aBrick globalBounds withWidth: 1) translateBy: (aBrick globalBounds origin negated x@0).
	
	^ myBounds intersects: testingBounds