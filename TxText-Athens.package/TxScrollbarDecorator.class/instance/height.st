geometry
height
	^ vertical
		ifFalse: [ scrollbar height ]
		ifTrue: [ super height ]