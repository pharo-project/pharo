tests - Float
testFloatReadError
	"This covers parsing in Number>>readFrom:"

	| rs num |
	rs := '1e' readStream.
	num := NumberParser parse: rs.
	self assert: 1 = num.
	self assert: rs upToEnd = 'e'.
	
	rs := '1s' readStream.
	num := NumberParser parse: rs.
	self assert: 1 = num.
	self assert: rs upToEnd = ''.

	rs := '1.' readStream.
	num := NumberParser parse: rs.
	self assert: 1 = num.
	self assert: num isInteger.
	self assert: rs upToEnd = '.'.
	
	rs := '' readStream.
	self should: [NumberParser parse: rs] raise: Error.
	
	rs := 'foo' readStream.
	self should: [NumberParser parse: rs] raise: Error.

	rs := 'radix' readStream.
	self should: [NumberParser parse: rs] raise: Error.
	
	rs := '.e0' readStream.
	self should: [NumberParser parse: rs] raise: Error.
	
	rs := '-.e0' readStream.
	self should: [NumberParser parse: rs] raise: Error.
	
	rs := '--1' readStream.
	self should: [NumberParser parse: rs] raise: Error.