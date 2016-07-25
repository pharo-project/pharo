instance creation
bytes: byteArray
	^ (self type: ZnMimeType applicationOctetStream length: byteArray size)
		bytes: byteArray;
		yourself 