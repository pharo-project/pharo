write and read
writeInteger: anInteger
	"PRIVATE -- Write the contents of a SmallInteger."

	byteStream nextInt32Put: anInteger	"signed!!!!!"