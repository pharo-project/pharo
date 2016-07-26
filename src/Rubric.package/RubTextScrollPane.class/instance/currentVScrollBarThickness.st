geometry
currentVScrollBarThickness
	^ self vScrollbarNeeded
		ifTrue: [ self scrollBarThickness ]
		ifFalse: [ 0 ]