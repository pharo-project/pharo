"
RBPatternBlockToken is the first-class representation of the pattern block token.


"
Class {
	#name : 'ASTPatternBlockToken',
	#superclass : 'ASTValueToken',
	#category : 'AST-Core-Tokens',
	#package : 'AST-Core',
	#tag : 'Tokens'
}

{ #category : 'class initialization' }
ASTPatternBlockToken class >> initialize [

	self deprecatedAliases: { #ASTPatternBlockToken }
]

{ #category : 'testing' }
ASTPatternBlockToken >> isPatternBlock [
	^true
]
