point functions
normal
	"Answer a Point representing the unit vector rotated 90 deg clockwise. For the zero point return -1@0."

	| n d |
	n := y negated @ x.
	(d := (n x * n x + (n y * n y))) = 0
		 ifTrue: [ ^ -1 @0 ].
	^ n / d sqrt