printing
numberOfDigits
	"Return how many digits are necessary to print this number in base 10.
	This does not count any place for minus sign, radix prefix or whatever."

	^ self numberOfDigitsInBase: 10 
