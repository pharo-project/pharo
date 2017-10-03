utilities
timestamp
	^Date today mmddyyyy, ' ',	
		((String streamContents: [:s | Time now print24: true on: s]) copyFrom: 1 to: 5)