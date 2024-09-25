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

{ #category : 'testing' }
ASTWarningNotice >> isWarning [

	^ true
]
