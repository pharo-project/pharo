Class {
	#name : #RBCodeSnippetScriptingTest,
	#superclass : #RBCodeSnippetTest,
	#category : #'AST-Core-Tests-Snippets'
}

{ #category : #tests }
RBCodeSnippetScriptingTest class >> testParameters [

	^ ParametrizedTestMatrix new
		  forSelector: #snippet addOptions: (RBCodeSnippet allSnippets select: [:each | each isScripting ]);
		  yourself
]
