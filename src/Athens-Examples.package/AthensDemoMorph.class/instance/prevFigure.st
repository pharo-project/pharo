accessing
prevFigure

	current := current - 1.
	
	current < 1 ifTrue: [ current := self figures size ].