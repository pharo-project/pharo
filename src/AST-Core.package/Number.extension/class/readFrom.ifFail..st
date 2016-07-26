*AST-Core-Parser
readFrom: stringOrStream ifFail: aBlock
	"Answer a number as described on aStream.  The number may
	be any accepted Smalltalk literal Number format.
	It can include a leading radix specification, as in 16rFADE.
	It can as well be NaN, Infinity or -Infinity for conveniency.
	If input does not represent a valid number, then execute fail block
	and leave the stream positioned before offending character"
	
	^(NumberParser on: stringOrStream) failBlock: aBlock; nextNumber