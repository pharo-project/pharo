writing
writeTRNSChunkOn: aStream
	"Write out tRNS chunk"
	aStream nextPutAll: 'tRNS' asByteArray.
	form colors do:[:aColor|
		aStream nextPut: (aColor alpha * 255) truncated.
	].