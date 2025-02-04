"
A simple example for a AST transforming compiler plugin.

I replace 42 with a  'meaning of life'.
"
Class {
	#name : 'OCPluginMeaningOfLife',
	#superclass : 'OCStaticASTCompilerPlugin',
	#category : 'OpalCompiler-Tests-Plugins',
	#package : 'OpalCompiler-Tests',
	#tag : 'Plugins'
}

{ #category : 'private' }
OCPluginMeaningOfLife class >> priority [
	^ 50
]

{ #category : 'private - transforming' }
OCPluginMeaningOfLife >> transform [

	| rule |
	"copy the AST as we modify it"
	self copyAST.
	"we use the ASTParseTreeRewriter to do the change, another option is to do it directly using
	e.g #replaceWith:"
	rule := OCParseTreeRewriter
		        replaceLiteral: 42
		        with: 'meaning of life'.
	rule executeTree: ast.
	^ ast
]
