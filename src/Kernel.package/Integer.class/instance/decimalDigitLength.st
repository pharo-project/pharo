accessing
decimalDigitLength

	"Return how many digits are necessary to print this number in base 10.
	This does not count any place for minus sign, radix prefix or whatever.
	Result is not defined from negative numbers."
	
	^ self numberOfDigitsInBase: 10