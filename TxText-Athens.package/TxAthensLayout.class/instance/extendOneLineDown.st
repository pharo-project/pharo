scanning
extendOneLineDown
	
	"Extend layout to the bottom, by scanning one more visual line.
	Answer true if new line was added due to scanning..
	or false, if there's nothing left to scan (end of text) "
	| pos newLine |
	
	pos := self endPosition.
	pos isAtEnd ifTrue: [ ^ false ].

	"if end position is at line separator, advance to next line"
	(pos span isLineSeparator and: [ pos isAtSpanStart ]) ifTrue: [  pos := pos nextPosition ].
	
	newLine := strategy scanSingleVisualLineFor: self startingAt: pos.
	lines add: newLine.
	extent := (extent x max: newLine width) @ (extent y + newLine height).
	
	^ true.