initialize-release
startFrom: anObject 
	backlinks := OrderedCollection new.
	self addBacklinkLevel.
	objectsLeft := OrderedCollection with: anObject.
	backlinks last at: anObject put: nil