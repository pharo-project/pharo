printing
fullPrintString
	"Answer a String whose characters are a description of the receiver."

	^ String streamContents: [:s | self printOn: s]