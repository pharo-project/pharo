tests - NaN behavior
testNaN4
   	"FloatTest new testNaN4"

	| dict |
	dict := Dictionary new.
	dict at: Float nan put: #NaN.
	self deny: (dict includes: Float nan).
	"as a NaN is not equal to itself, it can not be retrieved when it is used as a dictionary key"
