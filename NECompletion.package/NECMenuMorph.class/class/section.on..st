help text
section: aString on: aTextStream 
	aTextStream
		withAttributes: self sectionAttributes
		do: [aTextStream nextPutAll: aString].
	aTextStream cr