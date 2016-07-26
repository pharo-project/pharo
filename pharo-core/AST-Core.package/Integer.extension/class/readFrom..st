*AST-Core-Parser
readFrom: aStringOrStream 
	"Answer a new Integer as described on the stream, aStream.
	Embedded radix specifiers not allowed - use Number readFrom: for that."
	^self readFrom: aStringOrStream base: 10