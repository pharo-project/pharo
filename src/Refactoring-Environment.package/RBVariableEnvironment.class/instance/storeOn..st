printing
storeOn: aStream 
	aStream
		nextPut: $(;
		nextPutAll: self class name;
		nextPutAll: ' new '.
	self accessorMethods do: [ :each | 
		aStream
			nextPutAll: each;
			nextPutAll: ': '.
		(self perform: each) storeOn: aStream.
		aStream nextPutAll: '; '].
	aStream nextPutAll: 'yourself)'