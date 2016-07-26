*AST-Core-Parser
readFrom: stringOrStream 
	"Answer a number as described on aStream.  The number may
	be any accepted Smalltalk literal Number format.
	It can include a leading radix specification, as in 16rFADE.
	It can as well be NaN, Infinity or -Infinity for conveniency.
	If stringOrStream does not start with a valid number description, fail."
	
	^(NumberParser on: stringOrStream) nextNumber