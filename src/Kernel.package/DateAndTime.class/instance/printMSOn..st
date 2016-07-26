printing
printMSOn: aStream
	"Print just mm:ss"
	
	self minute printOn: aStream base: 10 length: 2 padded: true.
	aStream nextPut: $:.
	self second printOn: aStream base: 10 length: 2 padded: true