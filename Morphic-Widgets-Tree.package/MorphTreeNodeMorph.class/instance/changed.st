updating
changed
	"Need to invalidate the selection frame."
	container
		ifNil: [super changed] 
		ifNotNil: [container invalidRect: self selectionFrame]