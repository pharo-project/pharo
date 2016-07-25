write and read
writeString: aString
	"PRIVATE -- Write the contents of a String."

	byteStream nextStringPut: aString.