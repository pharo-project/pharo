Class {
	#name : #ASTCacheMissStrategy,
	#superclass : #Object,
	#category : #'AST-Core-Parser'
}

{ #category : #accessing }
ASTCacheMissStrategy >> getASTFor: aCompiledMethod [
	| ast |
	ast := aCompiledMethod parseTree doSemanticAnalysisIn: aCompiledMethod methodClass.
	ast compilationContext compiledMethod: aCompiledMethod.
	^ast
]
