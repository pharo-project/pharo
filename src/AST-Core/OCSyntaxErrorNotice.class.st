"
I am the implicit notice of a ASTParseErrorNode
"
Class {
	#name : 'OCSyntaxErrorNotice',
	#superclass : 'OCErrorNotice',
	#category : 'AST-Core-Notice',
	#package : 'AST-Core',
	#tag : 'Notice'
}

{ #category : 'testing' }
OCSyntaxErrorNotice >> isSyntaxError [

	^ true
]

{ #category : 'accessing' }
OCSyntaxErrorNotice >> messageText [

	^ node errorMessage
]

{ #category : 'error handling' }
OCSyntaxErrorNotice >> position [

	^ node errorPosition
]
