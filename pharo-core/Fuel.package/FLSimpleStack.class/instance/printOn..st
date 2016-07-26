printing
printOn: aStream 
	"Append a sequence of characters that identify the receiver to aStream."

	super printOn: aStream.
	array printElementsOn: aStream
