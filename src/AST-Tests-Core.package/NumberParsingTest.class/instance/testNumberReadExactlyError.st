tests - Float
testNumberReadExactlyError
	"This covers parsing in Number>>readExactlyFrom:"

	| rs |
	
	rs := '' readStream.
	self should: [Number readFrom: rs] raise: Error.
	
	rs := 'foo' readStream.
	self should: [Number readFrom: rs] raise: Error.

	rs := 'radix' readStream.
	self should: [Number readFrom: rs] raise: Error.
	
	rs := '.e0' readStream.
	self should: [Number readFrom: rs] raise: Error.
	
	rs := '-.e0' readStream.
	self should: [Number readFrom: rs] raise: Error.
	
	rs := '--1' readStream.
	self should: [Number readFrom: rs] raise: Error.