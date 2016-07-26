printing
printHMSOn: aStream separatedBy: aSeparator
	"Print just hh<aSep>mm<aSep>ss"

	self hour
		printOn: aStream
		base: 10
		length: 2
		padded: true.
	aStream nextPut: aSeparator.
	self minute
		printOn: aStream
		base: 10
		length: 2
		padded: true.
	aStream nextPut: aSeparator.
	self second
		printOn: aStream
		base: 10
		length: 2
		padded: true