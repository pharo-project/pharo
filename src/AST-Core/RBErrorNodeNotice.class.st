"
I am the implicit notice of a RBParseErrorNode
"
Class {
	#name : #RBErrorNodeNotice,
	#superclass : #RBErrorNotice,
	#category : #'AST-Core-Notice'
}

{ #category : #'error handling' }
RBErrorNodeNotice >> errorPosition [

	^ node errorPosition
]

{ #category : #accessing }
RBErrorNodeNotice >> messageText [

	^ node errorMessage
]
