"
This class contains tests

Parametrized matrix test class to run various test on code snippets.
The only parameter is the instance variable `snippet` that is the current code snippet <RBCodeSnippet>.
"
Class {
	#name : 'OCCodeSnippetTest',
	#superclass : 'ParametrizedTestCase',
	#instVars : [
		'snippet'
	],
	#category : 'AST-Core-Tests-Snippets',
	#package : 'AST-Core-Tests',
	#tag : 'Snippets'
}

{ #category : 'building suites' }
OCCodeSnippetTest class >> testParameters [

	^ ParametrizedTestMatrix new
		  forSelector: #snippet addOptions: OCCodeSnippet allSnippets;
		  yourself
]

{ #category : 'private' }
OCCodeSnippetTest >> performTest [

	self skipIf: testSelector asSymbol.
	^ super performTest
]

{ #category : 'asserting' }
OCCodeSnippetTest >> skipIf: aSymbol [

	snippet ifSkip: aSymbol then: [ self skip ]
]

{ #category : 'accessing' }
OCCodeSnippetTest >> snippet [

	^ snippet
]

{ #category : 'accessing' }
OCCodeSnippetTest >> snippet: anObject [

	snippet := anObject
]
