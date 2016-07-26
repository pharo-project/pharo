tests - Float
testFloatFromStreamAsNumber
	"This covers parsing in Number>>readFrom:"

	| rs aFloat |
	rs := '10r-12.3456' readStream.
	aFloat := Number readFrom: rs.
	self assert: -12.3456 = aFloat.
	self assert: rs atEnd.

	rs := '10r-12.3456e2' readStream.
	aFloat := Number readFrom: rs.
	self assert: -1234.56 = aFloat.
	self assert: rs atEnd.

	rs := '10r-12.3456e2e2' readStream.
	aFloat := Number readFrom: rs.
	self assert: -1234.56 = aFloat.
	self assert: rs upToEnd = 'e2'.

	rs := '10r-12.3456d2' readStream.
	aFloat := Number readFrom: rs.
	self assert: -1234.56 = aFloat.
	self assert: rs atEnd.

	rs := '10r-12.3456q2' readStream.
	aFloat := Number readFrom: rs.
	self assert: -1234.56 = aFloat.
	self assert: rs atEnd.

	rs := '-12.3456q2' readStream.
	aFloat := Number readFrom: rs.
	self assert: -1234.56 = aFloat.
	self assert: rs atEnd.

	rs := '12.3456q2' readStream.
	aFloat := Number readFrom: rs.
	self assert: 1234.56 = aFloat.
	self assert: rs atEnd.

	rs := '12.3456z2' readStream.
	aFloat := Number readFrom: rs.
	self assert: 12.3456 = aFloat.
	self assert: rs upToEnd = 'z2'.
