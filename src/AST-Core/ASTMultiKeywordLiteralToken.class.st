"
I am used for a multi-keyword literal, found in a literal array.
Scanning a message send with a multi-keyword selector produces not this token.
Instead, that token stream would contain multiple `ASTKeywordToken`s. 
But if a multikeyword selector is within a literal array `#(printOn:indent: )`
this single selector `printOn:indent:` is scanned as one `ASTMultiKeywordLiteralToken`(`#printOn:indent:`).

"
Class {
	#name : #ASTMultiKeywordLiteralToken,
	#superclass : #ASTLiteralToken,
	#category : #'AST-Core-Tokens'
}

{ #category : #testing }
ASTMultiKeywordLiteralToken >> isMultiKeyword [
	^true
]
