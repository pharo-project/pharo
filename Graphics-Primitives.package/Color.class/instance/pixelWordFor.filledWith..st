conversions
pixelWordFor: depth filledWith: pixelValue 
	"Return to a 32-bit word that concatenates enough copies of the given pixel value to fill the word (i.e., 32/depth copies). Depth should be one of 1, 2, 4, 8, 16, or 32. The pixel value should be an integer in 0..2^depth-1."
	| halfword |
	depth = 32 ifTrue: [ ^ pixelValue ].
	depth = 16 
		ifTrue: [ halfword := pixelValue ]
		ifFalse: 
			[ halfword := pixelValue * (#(
					65535
					21845
					#-
					4369
					#-
					#-
					#-
					257
				) at: depth)	"replicates at every bit"	"replicates every 2 bits"	"replicates every 4 bits"	"replicates every 8 bits" ].
	^ halfword bitOr: (halfword bitShift: 16)