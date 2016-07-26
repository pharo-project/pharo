tests - Float
testFloatFromStreamWithExponent
	"This covers parsing in Number>>readFrom:"

	| rs aFloat |
	rs := '1.0e-14' readStream.
	aFloat := Number readFrom: rs.
	self assert: 1.0e-14 = aFloat.
	self assert: rs atEnd.

	rs := '1.0e-14 1' readStream.
	aFloat := Number readFrom: rs.
	self assert: 1.0e-14 = aFloat.
	self assert: rs upToEnd = ' 1'.

	rs := '1.0e-14eee' readStream.
	aFloat := Number readFrom: rs.
	self assert: 1.0e-14 = aFloat.
	self assert: rs upToEnd = 'eee'.

	rs := '1.0e14e10' readStream.
	aFloat := Number readFrom: rs.
	self assert: 1.0e14 = aFloat.
	self assert: rs upToEnd = 'e10'.

	rs := '1.0e+14e' readStream.
	aFloat := Number readFrom: rs.
	self assert: 1.0 = aFloat.
	self assert: rs upToEnd = 'e+14e'.

	rs := '1.0e' readStream.
	aFloat := Number readFrom: rs.
	self assert: 1.0 = aFloat.
	self assert: rs upToEnd = 'e'.