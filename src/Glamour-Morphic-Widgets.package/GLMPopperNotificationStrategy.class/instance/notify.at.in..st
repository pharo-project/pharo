notification
notify: aString at: anInteger in: aStream
	| pos |
	pos := self editor selectionInterval notEmpty
		ifTrue: [self editor startIndex + anInteger - 1 ]
		ifFalse: [anInteger].
	self editor selectFrom: pos to: pos.
	
	GLMErrorPopper
		installAlarm: #popupError:
		for: self
		withArguments: { aString }