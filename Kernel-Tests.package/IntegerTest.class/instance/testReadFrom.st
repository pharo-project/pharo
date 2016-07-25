tests - instance creation
testReadFrom
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
	self assert: '.s could be confused with a ScaledDecimal' = s