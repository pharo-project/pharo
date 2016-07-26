tests - Float
testFloatmin
	"Note that these are originally tests cases for former bugs of libc dtoa from netlib.
	ref http://www.exploringbinary.com/gays-strtod-returns-zero-for-inputs-just-above-2-1075/
	ref http://gcc.gnu.org/viewcvs/gcc/trunk/gcc/testsuite/gcc.dg/float-exact-1.c?view=markup&pathrev=205119
	They are also non regression for a bug of NumberParser related to incorrect position of last non zero digit.
	ref https://pharo.fogbugz.com/f/cases/12642/bug-in-NumberParser-when-reading-a-number-with-fraction-part"
	| halfMin moreThanHalfmin |
	halfMin := NumberParser parse: (Float fmin asTrueFraction / 2 printShowingDecimalPlaces: 1 - Float fmin exponent).
	self assert: halfMin = 0.0 description: 'nearest even of 0.5*Float fmin is zero'.
	moreThanHalfmin := NumberParser parse: (Float fmin asTrueFraction / 2 + (10 raisedTo: Float fmin exponent - 4) printShowingDecimalPlaces: 4 - Float fmin exponent).
	self assert: moreThanHalfmin = Float fmin description: 'nearest Float of a Fraction > 0.5*Float fmin is Float fmin'.