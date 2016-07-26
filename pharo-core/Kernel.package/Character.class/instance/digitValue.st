accessing
digitValue
	self asInteger > 255
		ifTrue: [ ^ self characterSet digitValueOf: self ].
	^ DigitValues at: 1 + self asInteger