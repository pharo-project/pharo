"
This is a particular englobing node that is a literal byte array.
Exemple : #[ literal byte array node ]
Can be created by forgetting 
the closure : #[ array node .
Forgetting the opening will be assumed to be a block node.
"
Class {
	#name : 'OCLiteralByteArrayErrorNode',
	#superclass : 'OCEnglobingErrorNode',
	#category : 'AST-Core-Nodes - ErrorNodes',
	#package : 'AST-Core',
	#tag : 'Nodes - ErrorNodes'
}

{ #category : 'construction' }
OCLiteralByteArrayErrorNode class >> error: aToken withNodes: aCollection [
	"An ASTLiteralByteArrayErrorNode can only be created from the opening token by choice.
	 Since the closure is a bracket, the default assumption is to create an ASTBlockErrorNode.
	 This could be change by an analysis of prior nodes."
	"If the collection is empty, there is only the token in the error."
	^aCollection isEmpty
		ifTrue: [ self new contents: aCollection; start: aToken start; stop: aToken stop; errorMessage: ''']'' expected' ]
		ifFalse: [ self new contents: aCollection; start: aToken start; stop: aCollection last stop; errorMessage: ''']'' expected' ]
]

{ #category : 'testing' }
OCLiteralByteArrayErrorNode >> isLiteralByteArrayError [
	^true
]
