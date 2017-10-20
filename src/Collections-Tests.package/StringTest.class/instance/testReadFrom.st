tests
testReadFrom
	self 
		assert: (String readFrom: ($' asString, 'foo', $' asString) readStream)
		equals: 'foo'.
	self 
		assert: (String readFrom: ($' asString, 'this ', $' asString, $' asString, ' is embedded', $' asString) readStream)
		equals: 'this '' is embedded'.
	self 
		assert: (String readFrom: ('<whitespace>', $' asString, 'foo', $' asString, '<whitespace>') readStream)
		equals: 'foo'.
	self
		assert: (String readFrom: 'this '' is embedded' printString readStream)
		equals: 'this '' is embedded'