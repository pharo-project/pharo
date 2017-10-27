bytecode decoding
callPrimitiveCode
	"Answer the call primitive bytecode, if it exists in the encoder's bytecode set, or nil if not.
	 248	11111000 	iiiiiiii	mjjjjjjj	Call Primitive #iiiiiiii + (jjjjjjj * 256) m=1 means inlined primitive, no hard return after execution."
	^248