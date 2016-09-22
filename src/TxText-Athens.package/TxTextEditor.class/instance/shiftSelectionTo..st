selection
shiftSelectionTo: aPosition
	"extends selection such that
		if aPosition is less than start, start moves to aPosition,
		if aPosition is greater than end, end moves to aPosition,
		if aPosition is inside current selection, start moves to aPosition"
	self recordSelectionChange: [ |start end|
		start := selection start min: selection end.
		end := selection start max: selection end.
		aPosition < end
			ifTrue: [ start := aPosition ]
			ifFalse: [ end := aPosition ].
		selection start: start.
		selection end: end ]