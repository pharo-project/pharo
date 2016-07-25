streams-compatibility
binaryReadStream
	^ (FileStream onHandle: self)
		ifNil: [ self streamError ]
		ifNotNil: [ :stream | 
			stream
				binary;
				yourself ]