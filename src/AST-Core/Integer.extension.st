Extension { #name : #Integer }

{ #category : #'*AST-Core-Parser' }
Integer class >> readFrom: aStringOrStream [ 
	"Answer a new Integer as described on the stream, aStream.
	Embedded radix specifiers not allowed - use Number readFrom: for that."
	^self readFrom: aStringOrStream base: 10
]

{ #category : #'*AST-Core-Parser' }
Integer class >> readFrom: aStringOrStream ifFail: aBlock [
	"Answer an instance of one of the concrete subclasses if Integer. 
	Initial minus sign accepted.
	Imbedded radix specifiers not allowed;  use Number 
	class readFrom: for that.
	Execute aBlock if there are no digits."

	^(NumberParser on: aStringOrStream) nextIntegerBase: 10 ifFail: aBlock
]

{ #category : #'*AST-Core-Parser' }
Integer class >> readFrom: aStream radix: radix [
	"retained for compatibility for now. do not use"
	^self readFrom: aStream base: radix
]
