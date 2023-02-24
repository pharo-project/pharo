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
RBCodeSnippetTest >> testDoSemanticAnalysis [
	"We should test more than that"

	| ast |
	ast := snippet parse.
	ast doSemanticAnalysis
]

{ #category : #tests }
RBCodeSnippetTest >> testDump [

	| ast dump ast2 dump2 |
	self skipIf: #testDump.
	ast := snippet parse.
	dump := ast dump.
	ast2 := OpalCompiler new evaluate: dump.
	self assert: ast2 equals: ast.
	dump2 := ast2 dump.
	self assert: dump2 equals: dump
]

{ #category : #tests }
RBCodeSnippetTest >> testFormattedCode [

	self assert: snippet parse formattedCode equals: snippet formattedCode
]

{ #category : #tests }
RBCodeSnippetTest >> testParse [

	self assert: snippet parse isFaulty equals: snippet isFaulty
]

{ #category : #tests }
RBCodeSnippetTest >> testParseOnError [

	snippet isFaulty
		ifTrue: [
			RBParser
				parseExpression: snippet source
				onError: [ :message :parser :position | ^ self ].
			self signalFailure: 'An error is expected' ]
		ifFalse: [
			RBParser
				parseExpression: snippet source
				onError: [ :message | ^ self signalFailure: 'Error unexpected: ', message ] ]
]
