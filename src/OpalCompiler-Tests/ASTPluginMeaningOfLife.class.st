"
A simple example for a AST transforming compiler plugin.

I replace 42 with a  'meaning of life'.
"
Class {
	#name : #ASTPluginMeaningOfLife,
	#superclass : #OCCompilerASTPlugin,
	#category : #'OpalCompiler-Tests-Plugins'
}

{ #category : #accessing }
ASTPluginMeaningOfLife class >> priority [
	^ 0
]

{ #category : #transformation }
ASTPluginMeaningOfLife >> transform [
	| rule |
	rule := RBParseTreeRewriter replaceLiteral: 42 with: 'meaning of life'.
	rule executeTree: ast.
	^ast
]
