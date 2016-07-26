tests - Integer
testIntegerReadFrom
	"Ensure remaining characters in a stream are not lost when parsing an integer."
	| rs i s |
	rs := '123s could be confused with a ScaledDecimal' readStream.
	i := Integer readFrom: rs.
	self assert: i = 123.
	s := rs upToEnd.
	self assert: 's could be confused with a ScaledDecimal' = s.
	rs := '123.s could be confused with a ScaledDecimal' readStream.
	i := Integer readFrom: rs.
	self assert: i = 123.
	s := rs upToEnd.
	self assert: '.s could be confused with a ScaledDecimal' = s.
	rs := '123sA has unary message sA' readStream.
	i := Number readFrom: rs.
	self assert: i = 123.
	s := rs upToEnd.
	self assert: 'sA has unary message sA' = s.
	rs := '123sB has unary message sB' readStream.
	i := Number readFrom: rs.
	self assert: i = 123.
	s := rs upToEnd.
	self assert: 'sB has unary message sB' = s