writing
writeIENDChunkOn: aStream
	"Write the IEND chunk"
	aStream nextPutAll: 'IEND' asByteArray.