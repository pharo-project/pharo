nonhomogeneous accessing
nextInt32
	"Read a 32-bit signed integer from the next 4 bytes"
	| s |
	s := 0.
	1 to: 4 do: [:i | s := (s bitShift: 8) + self next].
	(s bitAnd: 16r80000000) = 0
		ifTrue: [^ s]
		ifFalse: [^ -1 - s bitInvert32]