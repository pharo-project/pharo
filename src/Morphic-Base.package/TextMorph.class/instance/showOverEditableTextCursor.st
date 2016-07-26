event handling
showOverEditableTextCursor
	| o |
	owner isNil
		ifTrue: [^ self].
	o := owner isWorldMorph 
		ifTrue: [self] 
		ifFalse: [owner].
	(o boundsInWorld containsPoint: self currentHand position)
		ifTrue: [self currentHand showTemporaryCursor: (self theme overTextCursorFor: self)]
