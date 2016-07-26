accessing
digitAt: anExponent base: base

	"Return number that represents digit at given position.
		42 digitAt: 2 base: 10 -> 4
		42 digitAt: 1 base: 10 -> 1
	It is always a number or zero:
		16rFF digitAt: 1 base: 16 -> 15
		1 digitAt: 2 base: 10 -> 0
	Results are not defined for base smaller than 2 and non-integer arguments."

	^ self // (base raisedToInteger: anExponent - 1) \\ base