"
I am a notice for syntaxic or semantic warnings
"
Class {
	#name : 'ASTWarningNotice',
	#superclass : 'ASTNotice',
	#category : 'AST-Core-Notice',
	#package : 'AST-Core',
	#tag : 'Notice'
}

{ #category : 'class initialization' }
ASTWarningNotice class >> initialize [

	self deprecatedAliases: { #ASTWarningNotice }
]

{ #category : 'testing' }
ASTWarningNotice >> isWarning [

	^ true
]
