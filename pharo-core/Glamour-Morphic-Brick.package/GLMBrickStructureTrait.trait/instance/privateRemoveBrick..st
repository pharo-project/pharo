private-structure
privateRemoveBrick: aBrick
	| predecessor successor link head tail index|
	aBrick ifNil: [ ^ aBrick ].
	(self hasSubbrick: aBrick) ifFalse: [ ^ aBrick ].
	
	link := aBrick brickDoubleLink.
	
	head := self firstSubbrick brickDoubleLink.
	tail := self lastSubbrick brickDoubleLink.
	
	predecessor := link previousLink.
	successor := link nextLink.
	predecessor
		ifNotNil: [ predecessor nextLink: successor ].
	successor
		ifNotNil: [ successor previousLink: predecessor ].
	link clearLinks.
	
	aBrick brickDoubleLink: nil.
	
	index := self subbricks find: aBrick.
	self privateSubbricks: (self subbricks copyWithoutIndex: index).
	aBrick owner: nil.
	
	^ aBrick