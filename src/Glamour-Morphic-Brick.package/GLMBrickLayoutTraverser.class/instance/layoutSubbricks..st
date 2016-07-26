private
layoutSubbricks: aBrick
	aBrick ifNil: [ ^ self ].
	aBrick isBrick ifFalse: [ ^ self ].
	((self isClean: aBrick) & (self isOppositeClean: aBrick))
		ifFalse: [ ^ self ].
	(self isLayoutClean: aBrick) ifTrue: [ ^ self ].
	(aBrick subbricks allSatisfy: [ :each | each isBrick ifTrue: [ (self isClean: each) & (self isOppositeClean: each) ] ifFalse: [ false ] ])
		ifFalse: [ ^ self ].
	
	aBrick invalidateLayout.
	self markLayoutClean: aBrick.
	aBrick onChildrenLayouted