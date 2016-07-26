accessing
actualCursorColor
	self textArea readOnly ifTrue: [ ^ Color transparent ].
	^ (self textArea hasFocus and: [ self stayVisible not ])
		ifFalse: [ self invisibleCursorColor ]
		ifTrue: [ self visibleCursorColor ]