Extension { #name : #String }

{ #category : #'*AST-Core-Parser' }
String >> asNumber [ 
	"Answer the Number created by interpreting the receiver as the string 
	representation of a number."

	^Number readFromString: self
]
