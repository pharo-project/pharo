event handling
mouseUp: evt

	self isMultipleSelection
		ifTrue: [ self mouseUpOnMultiple: evt ]
		ifFalse: [ self mouseUpOnSingle: evt ]