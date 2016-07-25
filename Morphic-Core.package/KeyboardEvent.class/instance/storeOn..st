printing
storeOn: aStream

	aStream nextPutAll: type.
	aStream space.
	self timeStamp storeOn: aStream.
	aStream space.
	position x asInteger storeOn: aStream.
	aStream space.
	position y asInteger storeOn: aStream.
	aStream space.
	buttons storeOn: aStream.
	aStream space.
	keyValue storeOn: aStream.
	aStream space.
	charCode storeOn: aStream.
	aStream space.
	scanCode storeOn: aStream.