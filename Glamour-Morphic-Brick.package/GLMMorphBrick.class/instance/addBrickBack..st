brick-structure-adding/removing
addBrickBack: aBrick
	|link|
	link := aBrick asDoubleLink.
	aBrick brickDoubleLink: link.
	
	self hasSubbricks ifTrue: [
		self lastSubbrick brickDoubleLink nextLink: link.	
		link previousLink: self lastSubbrick brickDoubleLink.
	].
	
	self privateSubbricks: (self subbricks copyWith: aBrick).
	aBrick privateOwner: self.