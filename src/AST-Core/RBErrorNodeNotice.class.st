"
I am the implicit notice of a RBParseErrorNode
"
Class {
	#name : #RBErrorNodeNotice,
	#superclass : #RBErrorNotice,
	#category : #'AST-Core-Notice'
}

{ #category : #accessing }
RBErrorNodeNotice >> messageText [

	^ node errorMessage
]

{ #category : #'error handling' }
RBErrorNodeNotice >> position [

	^ node errorPosition
]
