settings
insertionPointColor
	self focused ifFalse: [^ Color transparent].
	^ Display depth <= 2
		ifTrue: [Color black]
		ifFalse: [self class insertionPointColor]