geometry
width
	^ vertical
		ifTrue: [ scrollbar width ]
		ifFalse: [ super width ]