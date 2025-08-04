"
This is a particular englobing node that is a literal array.
Exemple : #( literal array node )
Can be created by forgetting 
the closure : #( array node .
Forgetting the opening will be assumed to be a parentheses node.
"
Class {
	#name : 'OCLiteralArrayErrorNode',
	#superclass : 'OCEnglobingErrorNode',
	#category : 'AST-Core-Nodes - ErrorNodes',
	#package : 'AST-Core',
	#tag : 'Nodes - ErrorNodes'
}

{ #category : 'construction' }
OCLiteralArrayErrorNode class >> error: aToken withNodes: aCollection [
	"An ASTLiteralArrayErrorNode can only be created from the opening token by choice.
	 Since the closure is a parentheses, the default assumption is to create an ASTParenthesesErrorNode.
	 This could be change by an analysis of prior nodes."
	"If the collection is empty, there is only the token in the error."
	^aCollection isEmpty
		ifTrue: [ self new contents: aCollection; start: aToken start; stop: aToken stop; errorMessage: ''')'' expected' ]
		ifFalse: [ self new contents: aCollection; start: aToken start; stop: aCollection last stop; errorMessage: ''')'' expected' ]
]

{ #category : 'testing' }
OCLiteralArrayErrorNode >> isLiteralArrayError [
	^true
]
