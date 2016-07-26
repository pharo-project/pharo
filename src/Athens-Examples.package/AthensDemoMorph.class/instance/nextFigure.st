accessing
nextFigure

	current := current + 1.
	
	current > self figures size ifTrue: [ current := 1 ].