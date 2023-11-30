Class {
	#name : 'ASTEOFToken',
	#superclass : 'ASTToken',
	#category : 'AST-Core-Tokens',
	#package : 'AST-Core',
	#tag : 'Tokens'
}

{ #category : 'class initialization' }
ASTEOFToken class >> initialize [

	self deprecatedAliases: { #ASTEOFToken }
]

{ #category : 'testing' }
ASTEOFToken >> isEOF [
	^true
]

{ #category : 'accessing' }
ASTEOFToken >> length [
	^ 0
]

{ #category : 'accessing' }
ASTEOFToken >> source [
	^ self value
]

{ #category : 'accessing' }
ASTEOFToken >> value [

	^ ''
]
