private
printProtocol: protocol sourceCode: sourceCode

	^ String streamContents: [ :stream |
		stream
			nextPutAll: '"protocol: '; 
			nextPutAll: protocol printString;
			nextPut: $"; cr; cr;
			nextPutAll: sourceCode ]	