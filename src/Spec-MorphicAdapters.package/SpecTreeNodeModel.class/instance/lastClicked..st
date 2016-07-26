protocol
lastClicked: aBoolean

	self nodeModel lastClicked: aBoolean.
	aBoolean
		ifTrue: [ self model highlightedItem: self nodeModel ]
		ifFalse: [ self model highlightedItem: nil ].