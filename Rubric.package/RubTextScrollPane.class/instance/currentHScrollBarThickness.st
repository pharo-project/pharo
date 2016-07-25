geometry
currentHScrollBarThickness
	^ self hScrollbarNeeded
		ifTrue: [ self scrollBarThickness ]
		ifFalse: [ 0 ]