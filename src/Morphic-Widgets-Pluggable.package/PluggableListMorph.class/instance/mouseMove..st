event handling
mouseMove: evt

	self isMultipleSelection
		ifTrue: [ self mouseMoveOnMultiple: evt ]
		ifFalse: [ self mouseMoveOnSingle:evt ]