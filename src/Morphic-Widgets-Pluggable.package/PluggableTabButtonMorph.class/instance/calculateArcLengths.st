precalculations
calculateArcLengths
	| array radius |
	radius := self cornerRadius.
	array := Array new: radius.
	
	1 to: radius do: [ :i | | x |
		x := i - 0.5.
		array at: i
		 	put: (radius - ((2 * x * radius) - (x * x)) sqrt) asInteger].
		
	self arcLengths: array