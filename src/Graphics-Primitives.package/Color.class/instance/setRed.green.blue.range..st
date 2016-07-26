private
setRed: r green: g blue: b range: range 
	"Initialize this color's r, g, and b components to the given values in the range [0..r]."
	rgb == nil ifFalse: [ self attemptToMutateError ].
	rgb := ((r * ComponentMask // range bitAnd: ComponentMask) bitShift: RedShift) + ((g * ComponentMask // range bitAnd: ComponentMask) bitShift: GreenShift) + (b * ComponentMask // range bitAnd: ComponentMask).
	cachedDepth := nil.
	cachedBitPattern := nil