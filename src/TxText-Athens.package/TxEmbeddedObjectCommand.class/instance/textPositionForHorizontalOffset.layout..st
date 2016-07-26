geometry
textPositionForHorizontalOffset: anOffset layout: layout

	^ anOffset < (self advance x/2) 
		ifTrue: [ span startPosition ]
		ifFalse: [ span endPosition ]
	