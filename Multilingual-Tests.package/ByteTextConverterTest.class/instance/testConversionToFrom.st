testing
testConversionToFrom
	"Non regresson test for http://code.google.com/p/pharo/issues/detail?id=986"

	self assert: (('äöü' convertToEncoding: 'mac-roman') convertFromEncoding: 'mac-roman') = 'äöü'