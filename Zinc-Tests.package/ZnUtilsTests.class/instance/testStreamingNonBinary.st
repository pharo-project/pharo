testing
testStreamingNonBinary
	| data in out |
	data := String streamContents: [ :stream | 
		1 to: 10000 do: [ :each | 
			stream nextPut: 'abc' atRandom ] ].
	in := data readStream.
	out := WriteStream on: (String new: data size).
	ZnUtils streamFrom: in to: out size: data size.
	self assert: in atEnd.
	self assert: out contents = data 