private-structure
privateAddBrickLinkBack: aBrick
	|link brick|
	
	brick := aBrick asBrick.
	link := brick asDoubleLink.
	brick brickDoubleLink: link.
	
	self hasSubbricks ifTrue: [
		self lastSubbrick brickDoubleLink nextLink: link.	
		link previousLink: self lastSubbrick brickDoubleLink ].
	
	brick owner: self.
	
	^ aBrick