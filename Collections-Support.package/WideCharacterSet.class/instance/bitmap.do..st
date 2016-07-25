private
bitmap: aMap do: aBlock
	"Execute a block with each value (0 based) corresponding to set bits.
	Implementation notes: this version works best for sparse maps.
	It has (byte lowBit) inlined for speed."
	
	| byte byteOffset lowBits |
	lowBits := #[1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 5 1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 6 1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 5 1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 7 1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 5 1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 6 1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 5 1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 8 1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 5 1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 6 1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 5 1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 7 1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 5 1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 6 1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 5 1 2 1 3 1 2 1 4 1 2 1 3 1 2 1]. "The lowBits table gives a 1-based bitOffset"
	1 to: aMap size do: [:i | 
		(byte := aMap at: i) = 0 ifFalse: [
			byteOffset := (i bitShift: 3) - 9. "This byteOffset is -1 based"
			["Evaluate the block with 0-based (byteOffset + bitOffset)"
			aBlock value: (byteOffset + (lowBits at: byte)).
			"Eliminate the low bit and loop if some bit remain"
			(byte := byte bitAnd: byte - 1) = 0] whileFalse]]