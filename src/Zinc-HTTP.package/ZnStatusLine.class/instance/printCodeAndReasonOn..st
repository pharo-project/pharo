printing
printCodeAndReasonOn: stream
	self code
		ifNotNil: [
			stream print: self code; space; nextPutAll: self reason ]