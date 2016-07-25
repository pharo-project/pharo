tests
testConvertFromInteger
	"Converting an Integer with asScaledDecimal use strictly necessary number of decimal places: 0."

	| sd |
	sd := 13 asScaledDecimal.
	self assert: 0 = sd scale.
	self assert: ('13s0' = sd printString).
	sd := -13 asScaledDecimal.
	self assert: 0 = sd scale.
	self assert: ('-13s0' = sd printString).
	sd := 130000000013 asScaledDecimal.
	self assert: 0 = sd scale.
	self assert: ('130000000013s0' = sd printString).
	sd := -130000000013 asScaledDecimal.
	self assert: 0 = sd scale.
	self assert: ('-130000000013s0' = sd printString)
