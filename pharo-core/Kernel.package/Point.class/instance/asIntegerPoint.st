converting
asIntegerPoint
	"Convert me to an integer point transforming both of my coordinates
	to integers using #asInteger. If x and y are already represented
	by integers return myself, otherwise create a new Point instance."
	self isIntegerPoint
		ifTrue: [ ^ self ].
	^ x asInteger @ y asInteger