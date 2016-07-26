printing
printWithCommasOn: aStream
	"123456789 asStringWithCommas"
	"-123456789 asStringWithCommas"
	^ self printSeparatedBy: $, every: 3 signed: false on: aStream