private-structure
privateAddBrick: aBrick before: otherBrick
	| brick link otherLink otherLinkPredeccessor index|
	brick := aBrick asBrick.
	
	otherLink := otherBrick brickDoubleLink.
	otherLink = self firstSubbrick brickDoubleLink
		ifTrue: [ ^ self privateAddBrickFirst: aBrick ].
		
	link := brick asDoubleLink.
	brick brickDoubleLink: link.
	otherLinkPredeccessor := otherLink previousLink.
	otherLink previousLink: link.
	link nextLink: otherLink.
	link previousLink: otherLinkPredeccessor.
	otherLinkPredeccessor nextLink: link.
	
	index := ((self subbricks indexOf: otherBrick) max: 1).
	self privateSubbricks: (self subbricks copyReplaceFrom: index to: index - 1 with: (Array with: brick)).
	
	brick owner: self.
	
	^ aBrick