"
End of file token
"
Class {
	#name : #ASTEOFToken,
	#superclass : #ASTToken,
	#category : #'AST-Core-Tokens'
}

{ #category : #testing }
ASTEOFToken >> isEOF [
	^true
]

{ #category : #accessing }
ASTEOFToken >> length [
	^ 0
]

{ #category : #accessing }
ASTEOFToken >> value [

	^ ''
]
