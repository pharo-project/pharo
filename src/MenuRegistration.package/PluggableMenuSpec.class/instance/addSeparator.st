construction
addSeparator
	self items isEmpty
		ifTrue: [ ^ nil ].
	self items last separator: true