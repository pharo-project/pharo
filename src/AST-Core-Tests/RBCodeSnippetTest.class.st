"
This class contains tests

Parametrized matrix test class to run various test on code snipets.
The only parameter is the instance variable `snipet` that is the current code snipet <RBCodeSnipet>.
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
RBCodeSnippetTest >> testCritiques [

	| ast critiques |
	ast := snippet parse.
	critiques := ast critiques.

	"Alone blocks will have ReDeadBlockRule. Currently no other critiques are fired."
	"When we get some critiques, we will add a instance vaviable in the snipet to validate them"
	ast isBlock
		ifTrue: [
			self assert: critiques size equals: 1.
			self assert: critiques anyOne rule class equals: ReDeadBlockRule ]
		ifFalse: [ self assert: critiques isEmpty ]
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
