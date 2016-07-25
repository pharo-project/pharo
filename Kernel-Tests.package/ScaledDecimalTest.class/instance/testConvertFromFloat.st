tests
testConvertFromFloat

	| aFloat sd f2 diff |
	aFloat := 11/13 asFloat.
	sd := aFloat asScaledDecimal: 2.
	self assert: 2 = sd scale.
	self assert: '0.85s2' = sd printString.
	self assert: '-0.85s2' = sd negated printString.
	f2 := sd asFloat.
	diff := f2 - aFloat.
	self assert: diff abs < 1.0e-9. "actually, f = f2, but this is not a requirement"
