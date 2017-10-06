printing
longPrintOn: aStream
	"Append to the argument, aStream, the names and values of all of the receiver's instance variables.  But, not useful for a class with a method dictionary."

	aStream nextPutAll: '<<too complex to show>>'; cr.