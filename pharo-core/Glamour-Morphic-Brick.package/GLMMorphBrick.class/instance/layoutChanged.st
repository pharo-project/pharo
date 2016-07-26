layout
layoutChanged
	super layoutChanged.
	
	self morph ifNil: [ ^ self ].
	self isShrinking ifFalse: [ ^ self ].
	isLayouting ifTrue: [ ^ self ].

	isLayouting := true.
	self changed.
	self isHShrinking
		ifTrue: [ self width: [ self shrinkingWidth + self hPadding ] ].
	self isVShrinking
		ifTrue: [ self height: [ self shrinkingHeight + self vPadding ] ].
		self invalidateMorphBounds: true.
	isLayouting := false.