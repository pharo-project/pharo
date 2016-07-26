private-structure
owner: aBrick
	|oldOwner|
	oldOwner := self owner.
	self privateOwner: aBrick.
	aBrick
		ifNotNil: [ self themerChanged ]
		ifNil: [ self onRemovedFrom: oldOwner ]