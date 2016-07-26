geometry etoy
setDirectionFrom: aPoint
	| delta degrees inner |
	inner := self renderedMorph.
	inner == self ifTrue:[^self].
	delta := (inner transformFromWorld globalPointToLocal: aPoint) - inner referencePosition.
	degrees := delta degrees + 90.0.
	self forwardDirection: (degrees \\ 360) rounded.
