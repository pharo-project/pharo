event handling
checkTimeoutFrom: aHand
	
	localStamp ifNil: [ localStamp := Time millisecondClockValue. ^ self ].
	
	(Time millisecondClockValue - localStamp ) > dblClickTime ifFalse: [ ^ self ].
	
	clickState == #firstClickDown ifTrue: [ 
		clickState := #firstClickTimedOut.
		dragSelector ifNotNil: [ ^ self ]. 
	].

	aHand resetClickState.
	self doubleClickTimeout.
	
	clickState == #firstClickTimedOut ifTrue:[ self click ]
	


