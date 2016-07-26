private
layout: aBrick

	aBrick isWidthDirty
		ifTrue: [ self widthTraverser layoutAround: aBrick inContext: nil ].
	aBrick isHeightDirty
		ifTrue: [ self heightTraverser layoutAround: aBrick inContext: nil ]