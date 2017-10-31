deprecated
fractions: fractionsOrNil offsets: offsetsOrNil

	| fractions offsets |
	self deprecated: 'Do not use this method. It forces to create spurious objects (rectangle or points) for nothing.'.
	fractions := fractionsOrNil ifNil: [0@0 extent: 0@0].
	offsets := offsetsOrNil ifNil: [0@0 extent: 0@0].
	
	^self
		topFraction: fractions top offset: offsets top;
		leftFraction: fractions left offset: offsets left;
		bottomFraction: fractions bottom offset: offsets bottom;
		rightFraction: fractions right offset: offsets right
