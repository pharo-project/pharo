tests
testConvertFromIntegerWithScale
	"Converting an Integer with asScaledDecimal: does now honour the scale passed as message argument."

	| sd |
	sd := 13 asScaledDecimal: 6.
	self assert: 6 = sd scale.
	self assert: ('13.000000s6' = sd printString).
	sd := -13 asScaledDecimal: 4.
	self assert: 4 = sd scale.
	self assert: ('-13.0000s4' = sd printString).
	sd := 130000000013 asScaledDecimal: 3.
	self assert: 3 = sd scale.
	self assert: ('130000000013.000s3' = sd printString).
	sd := -130000000013 asScaledDecimal: 1.
	self assert: 1 = sd scale.
	self assert: ('-130000000013.0s1' = sd printString)
