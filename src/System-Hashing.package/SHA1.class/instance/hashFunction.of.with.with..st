private
hashFunction: i of: x with: y with: z
	"Compute the hash function for the i-th step of the block hash loop. We number our steps 1-80, versus the 0-79 of the standard."
	"Details: There are four functions, one for each 20 iterations. The second and fourth are the same."

	i <= 20 ifTrue: [^ x copy bitAnd: y; bitOr: (x copy bitInvert; bitAnd: z)].
	i <= 40 ifTrue: [^ x copy bitXor: y; bitXor: z].
	i <= 60 ifTrue: [^ x copy bitAnd: y; bitOr: (x copy bitAnd: z); bitOr: (y copy bitAnd: z)].
	^ x copy bitXor: y; bitXor: z
