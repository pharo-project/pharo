tests
testProgressChangeByCurrentValue
	| job |
	job := Job new.
	job min: 1; max: 11.
	self assert: job progress equals: 0.0.
	job currentValue: 6.
	self assert: job progress equals: 0.5.
	job currentValue: 11.
	self assert: job progress equals: 1.0.