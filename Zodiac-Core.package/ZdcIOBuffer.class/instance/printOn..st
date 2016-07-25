printing
printOn: stream
	super printOn: stream.
	stream nextPut: $(.
	buffer
		ifNil: [ stream << 'closed' ]
		ifNotNil: [  
			self gapAtFront > 0 
				ifTrue: [ stream nextPut: $-; print: self gapAtFront; space ].
			stream print: self contents.
			self availableForWriting > 0
				ifTrue: [ stream space; nextPut: $+; print: self availableForWriting ] ].
	stream nextPut: $)