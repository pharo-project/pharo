tests
testProgress
	| job |
	job := Job new.
	self assert: job progress equals: 0.0.
	job min: 0; max: 1.
	self assert: job progress equals: 0.0.
	job currentValue: 1.
	self assert: job progress equals: 1.0.