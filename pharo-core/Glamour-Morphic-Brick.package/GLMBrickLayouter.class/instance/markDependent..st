private
markDependent: aBrick

	(aBrick isWidthDirty | aBrick isHeightDirty)
		ifTrue: [
			"we mark layout as dirty of brick itself"
			aBrick layout markDirty.
			"and its direct parent"
			aBrick owner ifNotNil: [ :o | o isBrick ifTrue: [ o layout markDirty ] ] ].

	aBrick isWidthDirty
		ifTrue: [ self widthTraverser markDirtyAround: aBrick inContext: nil ].
	aBrick isHeightDirty
		ifTrue: [ self heightTraverser markDirtyAround: aBrick inContext: nil ]
	