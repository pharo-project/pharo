scanning
extendOneLineUp
	
	| pos newLine extraLines h |
	
	pos := self startPosition.
	
	pos isAtStart ifTrue: [ ^ false ].
		
	pos := self startPosition moveUp moveToLineStart.
	
	extraLines := OrderedCollection new.
	
	[ 
		newLine := strategy scanSingleVisualLineFor: self startingAt: pos.	
		extraLines add: newLine.
		
		"if end position is at line separator, advance to next line"
		pos := newLine endPosition.
		(pos span isLineSeparator and: [ pos isAtSpanStart ]) ifTrue: [  pos := pos nextPosition ].
		pos = self startPosition
	] whileFalse.

	h := extent y.
	extraLines reverseDo: [ :line |
		lines addFirst: line.
		extent := (extent x max: line width) @ (extent y + line height).
	].
	
	view privSetVerticalOffset: view offset y + (extent y - h).
	
	^ true.