tests - Float
testFloatMaxAndMin
	"This covers parsing in Number>>readFrom:"

	| rs aFloat |
	rs := '2r0.0000000000000000000000000000000000000000000000000001e-1022'
		readStream.
	aFloat := NumberParser parse: rs.
	self assert: Float fminDenormalized = aFloat.
	self assert: rs atEnd.
	rs := '-2r0.0000000000000000000000000000000000000000000000000001e-1022'
		readStream.
	aFloat := NumberParser parse: rs.
	self assert: Float fminDenormalized negated = aFloat.
	self assert: rs atEnd.
	rs := '2r1.1111111111111111111111111111111111111111111111111111e1023'
		readStream.
	aFloat := NumberParser parse: rs.
	self assert: Float fmax = aFloat.
	self assert: rs atEnd