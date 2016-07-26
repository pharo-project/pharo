nonhomogeneous accessing
nextInt32Put: int32
	"Write a signed integer to the next 4 bytes"
	| pos |
	pos := int32 < 0
		ifTrue: [(0-int32) bitInvert32 + 1]
		ifFalse: [int32].
	1 to: 4 do: [:i | self nextPut: (pos digitAt: 5-i)].
	^ int32