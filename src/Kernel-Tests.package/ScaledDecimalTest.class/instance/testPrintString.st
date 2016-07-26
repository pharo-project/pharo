tests
testPrintString
	"The printed representation of a ScaledDecimal is rounded.
	Note that old implementation was truncated."

	| sd |
	sd := (13 / 11) asScaledDecimal: 6.
	self assert: ('1.181818s6' = sd printString).
	sd := (13 / 11) asScaledDecimal: 5.
	self assert: ('1.18182s5' = sd printString).
	sd := (13 / 11) asScaledDecimal: 5.
	self deny: ('1.18181s5' = sd printString)
