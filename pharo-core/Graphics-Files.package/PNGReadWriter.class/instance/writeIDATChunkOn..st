writing
writeIDATChunkOn: aStream 
	"Write the IDAT chunk"
	| z |
	aStream nextPutAll: 'IDAT' asByteArray.
	z := ZLibWriteStream on: aStream.
	form depth <= 8 
		ifTrue: [ self writeType3DataOn: z ]
		ifFalse: [ self writeType6DataOn: z ].
	self debugging ifTrue: 
		[ Transcript
			cr;
			nextPutAll: 'compressed size=';
			print: aStream position;
			nextPutAll: ' uncompressed size=';
			print: z position ]