private
expandedBlock: aByteArray 
	"Convert the given 64 byte buffer into 80 32-bit registers and answer the result."
	| out src v |
	out := Array new: 80.
	src := 1.
	1 
		to: 16
		do: 
			[ :i | 
			out 
				at: i
				put: (ThirtyTwoBitRegister new 
						loadFrom: aByteArray
						at: src).
			src := src + 4 ].
	17 
		to: 80
		do: 
			[ :i | 
			v := (out at: i - 3) copy.
			v
				bitXor: (out at: i - 8);
				bitXor: (out at: i - 14);
				bitXor: (out at: i - 16);
				leftRotateBy: 1.
			out 
				at: i
				put: v ].
	^ out