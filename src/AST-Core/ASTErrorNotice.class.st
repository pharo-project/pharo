"
I am a notice for syntaxic or semantic errors
"
Class {
	#name : 'ASTErrorNotice',
	#superclass : 'ASTNotice',
	#category : 'AST-Core-Notice',
	#package : 'AST-Core',
	#tag : 'Notice'
}

{ #category : 'testing' }
ASTErrorNotice >> isError [

	^ true
]
