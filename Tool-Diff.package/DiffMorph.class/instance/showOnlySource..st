accessing
showOnlySource: aBoolean

	showOnlySource = aBoolean ifTrue: [ ^ self ].
	self showOnlyDestination: false.
	showOnlySource := aBoolean.
	self updateMorphs