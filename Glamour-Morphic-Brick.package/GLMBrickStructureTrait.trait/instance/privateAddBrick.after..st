private-structure
privateAddBrick: aBrick after: otherBrick
	| brick link otherLink otherLinkSuccessor index|
	brick := aBrick asBrick.
	
	otherLink := otherBrick brickDoubleLink.
	otherLink = self lastSubbrick brickDoubleLink
		ifTrue: [ ^ self privateAddBrickBack: aBrick ].
		
	link := brick asDoubleLink.
	brick brickDoubleLink: link.
	otherLinkSuccessor := otherLink nextLink.
	otherLink nextLink: link.
	link previousLink: otherLink.
	link nextLink: otherLinkSuccessor.
	otherLinkSuccessor previousLink: link.
	
	index := (self subbricks indexOf: otherBrick) + 1.
	self privateSubbricks: (self subbricks copyReplaceFrom: index to: index - 1 with: (Array with: brick)).
	
	brick owner: self.
	
	^ aBrick