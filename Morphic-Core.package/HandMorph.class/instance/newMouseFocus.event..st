focus handling
newMouseFocus: aMorph event: event 
	aMorph ifNotNil: [targetOffset := event cursorPoint - aMorph position].
	^self newMouseFocus: aMorph