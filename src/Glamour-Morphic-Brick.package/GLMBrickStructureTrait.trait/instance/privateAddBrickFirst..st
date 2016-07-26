private-structure
privateAddBrickFirst: aBrick
	| link head tail brick|
	
	brick := aBrick asBrick.
	head := nil.
	tail := nil.
	self hasSubbricks ifTrue: [
		head := self firstSubbrick brickDoubleLink.
		tail := self lastSubbrick brickDoubleLink ].
	
	link := brick asDoubleLink.
	brick brickDoubleLink: link.
	link nextLink: head.
	head ifNotNil: [ head previousLink: link ].
	
	self privateSubbricks: (self subbricks copyWithFirst: brick).
	brick owner: self.
	
	^ aBrick