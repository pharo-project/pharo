"
I am the implicit notice of a RBParseErrorNode
"
Class {
	#name : #RBSyntaxErrorNotice,
	#superclass : #RBErrorNotice,
	#category : #'AST-Core-Notice'
}

{ #category : #accessing }
RBSyntaxErrorNotice >> messageText [

	^ node errorMessage
]

{ #category : #'error handling' }
RBSyntaxErrorNotice >> position [

	^ node errorPosition
]
