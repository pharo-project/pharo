geometry
referencePosition: aPosition
	"Move the receiver to match its reference position with aPosition"
	| newPos intPos |
	newPos := self position + (aPosition - self referencePosition).
	intPos := newPos asIntegerPoint.
	newPos = intPos 
		ifTrue:[self position: intPos]
		ifFalse:[self position: newPos].