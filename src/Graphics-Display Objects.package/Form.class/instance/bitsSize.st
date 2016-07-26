accessing
bitsSize
	| pixPerWord |
	depth == nil ifTrue: [depth := 1].
	pixPerWord := 32 // self depth.
	^ width + pixPerWord - 1 // pixPerWord * height