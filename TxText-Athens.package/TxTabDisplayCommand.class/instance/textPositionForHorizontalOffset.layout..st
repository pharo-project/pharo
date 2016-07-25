geometry
textPositionForHorizontalOffset: anOffset layout: layout

	"Tab is a single unbreakable space, return start if offset closer to tab starting position, or end otherwise"
	^ anOffset < (advance x/2) 
		ifTrue: [ span positionAt: start ]
		ifFalse: [ span positionAt: stop ]
