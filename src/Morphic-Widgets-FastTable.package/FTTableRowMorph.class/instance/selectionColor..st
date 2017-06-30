accessing
selectionColor: aColor

	selectionColor = aColor ifTrue: [ ^ self ].
	selectionColor := aColor.
	self changed