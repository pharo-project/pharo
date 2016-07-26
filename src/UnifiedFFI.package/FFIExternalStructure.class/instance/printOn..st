printing
printOn: aStream 
	"Append to the argument, aStream, the names and values of all the record's variables."
	aStream nextPutAll: self class name; nextPutAll: ' ( '; cr.
	self class fieldSpec fieldNames do: [ :field |
		aStream nextPutAll: field; nextPut: $:; space; tab.
			(self perform: field ) printOn: aStream.
			] separatedBy: [ aStream cr ].
	aStream cr; nextPut: $)