scrollpane-testing
isVisible: aBrick
	|myBounds testingBounds|
	"need to normalize bounds to take only y coordinate into account"
	
	myBounds := (self allowedBounds withWidth: 0) translateBy: (self allowedBounds origin negated x@0).
	testingBounds :=  (aBrick globalBounds withWidth: 0) translateBy: (aBrick globalBounds origin negated x@0).
	
	^ myBounds containsRect: testingBounds