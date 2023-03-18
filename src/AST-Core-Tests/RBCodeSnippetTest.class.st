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
RBCodeSnippetTest >> testMonticello [

	| definition |

	self flag: 'Should be an extention method in Monticello-Test, but package dependency is garbage.'.

	"Force non method to be inside a method"
	snippet isMethod ifFalse: [
		snippet := snippet copy.
		snippet source: 'foo ' , snippet source ].

	definition := MCMethodDefinition
		className: #MCMockClassE
		classIsMeta: false
		selector: #'' "do not care, load will repear it"
		category: nil
		timeStamp: nil
		source: snippet source.

	"MCMockClassE should have no method. cleanup if some previous failed tests invalidated that"
	MCMockClassE methods copy do: [ :m | MCMockClassE removeSelector: m selector ].
	self assert: MCMockClassE methods size equals: 0.

	snippet isFaulty ifTrue: [
			self should: [ definition load ] raise: CodeError.
			self assert: MCMockClassE methods size equals: 0.
			^ self ].

	[ definition load ] on: Warning do: [ :e | e resume ]. "Ignore Selector missmatches"
	self assert: MCMockClassE methods size equals: 1.

	"forced methods cannot be executed, because no return (pharo language is weird)"
	snippet isMethod ifTrue: [ self testExecute: MCMockClassE >> definition selector ].

	definition unload.
	self assert: MCMockClassE methods size equals: 0
]

{ #category : #tests }
RBCodeSnippetTest >> testParse [

	self assert: snippet parse isFaulty equals: (snippet isParseFaulty ifNil: [snippet isFaulty])
]

{ #category : #tests }
RBCodeSnippetTest >> testParseOnError [

	| ast error |
	error := nil.

	ast := snippet parseOnError: [ :e | error := e messageText ].

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
