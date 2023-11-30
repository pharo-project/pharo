"
I am the implicit notice of a RBParseErrorNode
"
Class {
	#name : 'ASTSyntaxErrorNotice',
	#superclass : 'ASTErrorNotice',
	#category : 'AST-Core-Notice',
	#package : 'AST-Core',
	#tag : 'Notice'
}

{ #category : 'class initialization' }
ASTSyntaxErrorNotice class >> initialize [

	self deprecatedAliases: { #ASTSyntaxErrorNotice }
]

{ #category : 'testing' }
ASTSyntaxErrorNotice >> isSyntaxError [

	^ true
]

{ #category : 'accessing' }
ASTSyntaxErrorNotice >> messageText [

	^ node errorMessage
]

{ #category : 'error handling' }
ASTSyntaxErrorNotice >> position [

	^ node errorPosition
]
