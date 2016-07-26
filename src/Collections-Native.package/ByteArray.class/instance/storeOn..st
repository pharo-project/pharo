printing
storeOn: aStream
	aStream nextPutAll: '#['.
	self
		do: [ :each | each storeOn: aStream ]
		separatedBy: [ aStream nextPut: $ ].
	aStream nextPut: $]