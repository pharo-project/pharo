accessing
authorityWithPort
	^ String streamContents: [ :stream | 
			stream nextPutAll: self host; nextPut: $:; print: self portOrDefault ] 