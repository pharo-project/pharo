printing
storeArrayOn: aStream

	aStream nextPutAll: '#('.
	self storeArrayValuesOn: aStream.
	aStream nextPutAll: ') '
