testing
testReadUpToEndNonBinary
	| data |
	data := String streamContents: [ :stream | 
		1 to: 10000 do: [ :each | 
			stream nextPut: 'abc' atRandom ] ].
	self assert: (ZnUtils readUpToEnd: data readStream limit: nil) = data.
	self assert: (ZnUtils readUpToEnd: data readStream limit: 10000) = data.
	self should: [ ZnUtils readUpToEnd: data readStream limit: 9999 ] raise: ZnEntityTooLarge 
