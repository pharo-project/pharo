Class {
	#name : 'ASTCodeSnippetScriptingTest',
	#superclass : 'ASTCodeSnippetTest',
	#category : 'AST-Core-Tests-Snippets',
	#package : 'AST-Core-Tests',
	#tag : 'Snippets'
}

{ #category : 'tests' }
ASTCodeSnippetScriptingTest class >> testParameters [

	^ ParametrizedTestMatrix new
		  forSelector: #snippet addOptions: (ASTCodeSnippet allSnippets select: [:each | each isScripting ]);
		  yourself
]
