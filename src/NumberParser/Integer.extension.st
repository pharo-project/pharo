Extension { #name : #Integer }

{ #category : #'*NumberParser' }
Integer class >> readFrom: aStringOrStream [ 
	"Answer a new Integer as described on the stream, aStream.
	Embedded radix specifiers not allowed - use Number readFrom: for that."
	^self readFrom: aStringOrStream base: 10
]

{ #category : #'*NumberParser' }
Integer class >> readFrom: aStringOrStream base: base [ 
	"Answer an instance of one of the concrete subclasses if Integer. 
	Initial minus sign accepted, and bases > 10 use letters A-Z.
	Imbedded radix specifiers not allowed;  use Number 
	class readFrom: for that.
	Raise an Error if there are no digits.
	If stringOrStream dos not start with a valid number description, answer 0 for backward compatibility. This is not clever and should better be changed."

	^(NumberParser on: aStringOrStream) nextIntegerBase: base
]

{ #category : #'*NumberParser' }
Integer class >> readFrom: aStringOrStream ifFail: aBlock [
	"Answer an instance of one of the concrete subclasses if Integer. 
	Initial minus sign accepted.
	Imbedded radix specifiers not allowed;  use Number 
	class readFrom: for that.
	Execute aBlock if there are no digits."

	^(NumberParser on: aStringOrStream) nextIntegerBase: 10 ifFail: aBlock
]

{ #category : #'*NumberParser' }
Integer class >> readFrom: aStream radix: radix [
	"retained for compatibility for now. do not use"
	^self readFrom: aStream base: radix
]
