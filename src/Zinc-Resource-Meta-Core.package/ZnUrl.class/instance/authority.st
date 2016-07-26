accessing
authority
	^ self hasNonDefaultPort 
		ifTrue: [ 
			String streamContents: [ :stream | 
				stream nextPutAll: self host; nextPut: $:; print: self port ] ]
		ifFalse: [ 
			self host ]