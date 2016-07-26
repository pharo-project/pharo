version protocol
summary
	^ String
		streamContents: [ :s | 
			s nextPutAll: info summaryHeader.
			s
				cr;
				cr;
				nextPutAll: info message ]