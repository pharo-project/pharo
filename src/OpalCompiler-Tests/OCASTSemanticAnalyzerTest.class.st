Class {
	#name : 'OCASTSemanticAnalyzerTest',
	#superclass : 'TestCase',
	#category : 'OpalCompiler-Tests-Semantic',
	#package : 'OpalCompiler-Tests',
	#tag : 'Semantic'
}

{ #category : 'tests' }
OCASTSemanticAnalyzerTest >> testWhileWithArg [
	"If we analyse #whileTrue with a block that has an argument, answer false for #isInlineWhile.
	executing this code would lead to a runtime error, which is what we expect"
	| ast |
	ast := OCParser parseExpression: '[:test | ] whileTrue'.
	self deny: ast sendNodes first isInlineWhile
]
