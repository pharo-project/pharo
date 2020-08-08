"
I am used for a multi-keyword literal, found in a literal array.
Scanning a message send with a multi-keyword selector produces not this token.
Instead, that token stream would contain multiple `RBKeywordToken`s. 
But if a multikeyword selector is within a literal array `#(printOn:indent: )`
this single selector `printOn:indent:` is scanned as one `RBMultiKeywordLiteralToken`(`#printOn:indent:`).

"
Class {
	#name : #RBMultiKeywordLiteralToken,
	#superclass : #RBLiteralToken,
	#category : #'AST-Core-Tokens'
}

{ #category : #testing }
RBMultiKeywordLiteralToken >> isMultiKeyword [
	^true
]
