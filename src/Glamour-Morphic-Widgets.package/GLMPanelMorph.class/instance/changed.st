change reporting
changed
	"Report that the area occupied by this morph should be redrawn.
	Fixed to include submorphs outside the outerBounds."

	self world
		ifNil: [ ^ self ].

 	^ fullBounds 
		ifNil: [ self invalidRect: self bounds ]
		ifNotNil: [ self invalidRect: fullBounds ]