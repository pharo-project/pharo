private
setRed: r green: g blue: b 
	"Initialize this color's r, g, and b components to the given values in the range [0.0..1.0].  Encoded in a single variable as 3 integers in [0..1023]."
	rgb == nil ifFalse: [ self attemptToMutateError ].
	rgb := (((r * ComponentMax) rounded bitAnd: ComponentMask) bitShift: RedShift) + (((g * ComponentMax) rounded bitAnd: ComponentMask) bitShift: GreenShift) + ((b * ComponentMax) rounded bitAnd: ComponentMask).
	cachedDepth := nil.
	cachedBitPattern := nil