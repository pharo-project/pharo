tests
testStart
	self assert: aTimespan start =   jan01.
	aTimespan start: jan08.
	self assert: aTimespan start =   jan08.