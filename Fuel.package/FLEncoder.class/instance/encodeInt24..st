encoding
encodeInt24: aSmallInteger 
	"Write a signed integer to the next 4 bytes"
	| pos |
	pos := aSmallInteger < 0
		ifTrue: [((0-aSmallInteger) bitXor: 16rFFFFFF) + 1]
		ifFalse: [aSmallInteger].
	1 to: 3 do: [:i | stream nextPut: (pos digitAt: 4-i)].
