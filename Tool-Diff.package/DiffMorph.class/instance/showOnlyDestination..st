accessing
showOnlyDestination: aBoolean

	showOnlyDestination = aBoolean ifTrue: [ ^ self ].
	self showOnlySource: false.
	showOnlyDestination := aBoolean.
	self updateMorphs