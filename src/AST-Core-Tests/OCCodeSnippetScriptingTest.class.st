Class {
	#name : 'OCCodeSnippetScriptingTest',
	#superclass : 'OCCodeSnippetTest',
	#category : 'AST-Core-Tests-Snippets',
	#package : 'AST-Core-Tests',
	#tag : 'Snippets'
}

{ #category : 'tests' }
OCCodeSnippetScriptingTest class >> testParameters [

	^ ParametrizedTestMatrix new
		  forSelector: #snippet addOptions: (OCCodeSnippet allSnippets select: [:each | each isScripting ]);
		  yourself
]
