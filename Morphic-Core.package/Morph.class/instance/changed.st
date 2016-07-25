recategorized
changed
	"Report that the area occupied by this morph should be redrawn.
	Fixed to include submorphs outside the outerBounds."
	
	^fullBounds 
		ifNil: [self invalidRect: self privateFullBounds]
		ifNotNil: [self invalidRect: fullBounds]