testing-byteCode limits
testTooManyArguments
	self setUpForErrorsIn: '^ ` Too many arguments ->`[:x1 :x2 :x3 :x4 :x5 :x6 :x7 :x8 :x9 :x10 :x11 :x12 :x13 :x14 :x15 :x16 :x17 | ]'.
	self enumerateAllSelections.