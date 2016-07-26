printing
printWithCommasSignedOn: aStream
	"123456789 asStringWithCommasSigned"
	"-123456789 asStringWithCommasSigned"
	^ self printSeparatedBy: $, every: 3 signed: true on: aStream