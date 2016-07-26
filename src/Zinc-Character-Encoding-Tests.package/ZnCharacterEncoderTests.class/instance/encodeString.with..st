private
encodeString: string with: encoder
	^ ByteArray streamContents: [ :stream |
		string do: [ :each |
			encoder nextPut: each toStream: stream ] ]