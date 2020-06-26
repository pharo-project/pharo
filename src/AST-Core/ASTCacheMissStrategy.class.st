Class {
	#name : #ASTCacheMissStrategy,
	#superclass : #Object,
	#category : #'AST-Core-Parser'
}

{ #category : #accessing }
ASTCacheMissStrategy >> getASTFor: aCompiledMethod [
	^ aCompiledMethod parseTree doSemanticAnalysisIn: aCompiledMethod methodClass
]
