conversions
initializePrivateRed: r green: g blue: b 
	"Initialize this color's r, g, and b components to the given values in the range [0..ComponentMax].  Encoded in a single variable as 3 integers in [0..1023]."
	rgb == nil ifFalse: [ self attemptToMutateError ].
	rgb := ((r 
		min: ComponentMask
		max: 0) bitShift: RedShift) + ((g 
			min: ComponentMask
			max: 0) bitShift: GreenShift) + (b 
			min: ComponentMask
			max: 0).
	cachedDepth := nil.
	cachedBitPattern := nil