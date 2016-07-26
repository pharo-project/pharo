private
processBufferUsingPrimitives: aByteArray 
	"Process given 64-byte buffer using the primitives, accumulating the results in totals."
	"expand and process the buffer"
	| w |
	w := WordArray new: 80.
	self 
		primExpandBlock: aByteArray
		into: w.
	self 
		primHashBlock: w
		using: totals