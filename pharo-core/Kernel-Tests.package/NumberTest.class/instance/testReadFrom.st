tests
testReadFrom
	
	self assert: 1.0e-14	= (Number readFrom: '1.0e-14').
	self assert: 2r1e26	= (Number readFrom: '2r1e26').
	self should: [Number readFrom: 'foo'] raise: Error