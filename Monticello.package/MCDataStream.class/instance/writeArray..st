write and read
writeArray: anArray
	"PRIVATE -- Write the contents of an Array."

	byteStream nextNumber: 4 put: anArray size.
	self nextPutAll: anArray.