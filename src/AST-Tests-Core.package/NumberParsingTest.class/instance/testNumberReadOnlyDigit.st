tests - Float
testNumberReadOnlyDigit
	"This covers parsing in Number>>readFrom:"

	| rs num |
	rs := '1e' readStream.
	num := Number readFrom: rs.
	self assert: 1 = num.
	self assert: rs upToEnd = 'e'.
	
	rs := '1s' readStream.
	num := Number readFrom: rs.
	self assert: 1 = num.
	self assert: rs upToEnd = ''.

	rs := '1.' readStream.
	num := Number readFrom: rs.
	self assert: 1 = num.
	self assert: num isInteger.
	self assert: rs upToEnd = '.'.