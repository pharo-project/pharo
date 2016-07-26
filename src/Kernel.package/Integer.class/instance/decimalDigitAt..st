accessing
decimalDigitAt: anExponent

	"Return number that represents digit at given decimal position.
		42 digitAt: 2 base: 10 -> 4
		42 digitAt: 1 base: 10 -> 1
	It is always a number or zero:
		1 digitAt: 2 base: 10 -> 0
	Results are not defined non-integer arguments."

	^ self digitAt: anExponent base: 10