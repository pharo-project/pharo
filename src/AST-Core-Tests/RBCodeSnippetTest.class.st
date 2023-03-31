"
This class contains tests

Parametrized matrix test class to run various test on code snippets.
The only parameter is the instance variable `snippet` that is the current code snippet <RBCodeSnippet>.
"
Class {
	#name : #RBCodeSnippetTest,
	#superclass : #ParametrizedTestCase,
	#instVars : [
		'snippet'
	],
	#category : #'AST-Core-Tests-Snippets'
}

{ #category : #'building suites' }
RBCodeSnippetTest class >> testParameters [

	^ ParametrizedTestMatrix new
		  forSelector: #snippet addOptions: RBCodeSnippet allSnippets;
		  yourself
]

{ #category : #private }
RBCodeSnippetTest >> performTest [

	self skipIf: testSelector asSymbol.
	^ super performTest
]

{ #category : #asserting }
RBCodeSnippetTest >> skipIf: aSymbol [

	snippet ifSkip: aSymbol then: [ self skip ]
]

{ #category : #accessing }
RBCodeSnippetTest >> snippet [

	^ snippet
]

{ #category : #accessing }
RBCodeSnippetTest >> snippet: anObject [

	snippet := anObject
]

{ #category : #tests }
RBCodeSnippetTest >> testDump [

	| ast dump ast2 dump2 |
	ast := snippet parse.
	dump := ast dump.
	ast2 := OpalCompiler new evaluate: dump.
	self assert: ast2 equals: ast.
	dump2 := ast2 dump.
	self assert: dump2 equals: dump
]

{ #category : #tests }
RBCodeSnippetTest >> testFormattedCode [

	| ast |
	ast := snippet parse.
	self assert: ast formattedCode withSeparatorsCompacted equals: snippet formattedCode withSeparatorsCompacted
]

{ #category : #tests }
RBCodeSnippetTest >> testParse [

	self assert: snippet parse isFaulty equals: (snippet isParseFaulty ifNil: [snippet isFaulty])
]

{ #category : #tests }
RBCodeSnippetTest >> testParseOnError [

	| ast error |
	error := nil.

	ast := snippet parseOnError: [ :e | error := e errorMessage ].

	(snippet isParseFaulty ifNil: [ snippet isFaulty ])
		ifTrue: [ self assert: error isNotNil ]
		ifFalse: [
			self assert: error isNil.
			self deny: ast isFaulty ]
]

{ #category : #tests }
RBCodeSnippetTest >> testSimpleFormattedCode [

	| ast fast east |
	ast := snippet parse.
	fast := (RBSimpleFormatter format: ast) reject: #isSeparator.
	east := snippet formattedCode reject: #isSeparator.
	self assert: fast  equals: east
]
