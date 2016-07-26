private
expandedBlock: aByteArray
	"Convert the given 64 byte buffer into 80 32-bit registers and answer the result." 
	| out src |
	out := Array new: 64.
	src := 1.
	1 to: 16 do: [:i |
		out at: i put: (ThirtyTwoBitRegister new loadFrom: aByteArray at: src).
		src := src + 4].

	17 to: 64 do: [:i | | s0 s1 wi15 wi2 wi |
		wi15 := (out at: i - 15) copy.
		wi2 := (out at: i-2) copy.
		s0 := wi15 copy.
		s0 leftRotateBy: -7;
			bitXor: (wi15 copy leftRotateBy: -18);
			bitXor: (wi15 copy bitShift: -3).
		s1 := wi2 copy.
		s1 leftRotateBy: -17;
			bitXor: (wi2 copy leftRotateBy: -19);
			bitXor: (wi2 copy bitShift: -10).
		wi := (out at: i-16) copy += s0 += (out at: i-7) copy += s1.
		out at: i put: wi].
	^ out
