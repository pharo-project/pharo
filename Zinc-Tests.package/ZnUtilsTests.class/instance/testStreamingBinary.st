testing
testStreamingBinary
	| data in out |
	data := ByteArray streamContents: [ :stream | 
		1 to: 10000 do: [ :each |
			stream nextPut: #(1 2 3) atRandom ] ].
	in := data readStream.
	out := WriteStream on: (ByteArray new: data size).
	ZnUtils streamFrom: in to: out size: data size.
	self assert: in atEnd.
	self assert: out contents = data 