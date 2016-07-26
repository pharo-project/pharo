geometry
textPositionAtHorizontalOffset: anOffset 
	| advance |
	anOffset < 0 ifTrue:[ ^ self startPosition ].
	advance := 0.
	commands do: [:cmd | 
		(cmd advance x + advance > anOffset)
			ifTrue:[ ^ cmd textPositionForHorizontalOffset: anOffset - advance layout: layout].
		advance := advance + cmd advance x ].
	^ self endPosition