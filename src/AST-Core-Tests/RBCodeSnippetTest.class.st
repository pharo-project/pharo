"
This class contains tests

Parametrized matrix test class to run various test on code snippets.
The only parameter is the instance variable `snippet` that is the current code snippet <RBCodeSnippet>.
"
Class {
	#name : 'RBCodeSnippetTest',
	#superclass : 'ParametrizedTestCase',
	#instVars : [
		'snippet'
	],
	#category : 'AST-Core-Tests-Snippets',
	#package : 'AST-Core-Tests',
	#tag : 'Snippets'
}

{ #category : 'building suites' }
RBCodeSnippetTest class >> testParameters [

	^ ParametrizedTestMatrix new
		  forSelector: #snippet addOptions: RBCodeSnippet allSnippets;
		  yourself
]

{ #category : 'private' }
RBCodeSnippetTest >> performTest [

	self skipIf: testSelector asSymbol.
	^ super performTest
]

{ #category : 'asserting' }
RBCodeSnippetTest >> skipIf: aSymbol [

	snippet ifSkip: aSymbol then: [ self skip ]
]

{ #category : 'accessing' }
RBCodeSnippetTest >> snippet [

	^ snippet
]

{ #category : 'accessing' }
RBCodeSnippetTest >> snippet: anObject [

	snippet := anObject
]

{ #category : 'tests' }
RBCodeSnippetTest >> testCodeImporter [

	| string importer class runBlock |
	"Code importer meed a plain expression or use a custom format"
	snippet source isAllSeparators ifTrue: [ ^ self skip ].
	[
	string := snippet isScripting
		          ifFalse: [
			          class := ChunkImportTestCase new importAClass.
			          '!{1} methodsFor: ''some protocol''!{2}' format: {
					          class name asString.
					          snippet source } ]
		          ifTrue: [ snippet source ].

	"Note: it might be possible that the snipped messes with the chuck format... to investigate"
	importer := CodeImporter fromString: string.
	importer parseChunks.

	self skipIf: #exec.

	"Importer should fail when faulty"
	snippet isFaulty ifTrue: [
		self should: [ importer evaluate ] raise: CodeError.
		snippet isScripting ifFalse: [ class removeFromSystem ].
		^ self ].

	"When not faulty, it's more complicated..."
	runBlock := [
	            | value |
	            value := importer evaluate.

	            snippet isScripting ifFalse: [
		            | method phonyArgs |
		            self assert: value isSymbol.
		            "Need to call the method, the importer added it to `class`, so retrieve it"
		            method := class >> value.
		            class removeFromSystem. "No more needed"
		            phonyArgs := (1 to: method numArgs) asArray.
		            value := nil withArgs: phonyArgs executeMethod: method ].

	            value ].

	self testExecuteBlock: runBlock ] ensure: [ self packageOrganizer removePackage: ChunkImportTestCase new packageNameForTests ]
]

{ #category : 'tests' }
RBCodeSnippetTest >> testFormattedCode [

	| ast |
	ast := snippet parse.
	self assert: ast formattedCode withSeparatorsCompacted equals: snippet formattedCode withSeparatorsCompacted
]

{ #category : 'tests' }
RBCodeSnippetTest >> testMonticello [

	| definition |

	self flag: 'Should be an extention method in Monticello-Test, but package dependency is garbage.'.

	"Force non method to be inside a method"
	snippet isScripting ifTrue: [
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

	snippet isFaultyMinusUndeclared ifTrue: [
			self should: [ 	[ definition load ] on: Warning do: [ :e | e resume ]. "Ignore Selector missmatches" ] raise: CodeError.
			self assert: MCMockClassE methods size equals: 0.
			^ self ].

	[ definition load ] on: Warning do: [ :e | e resume ]. "Ignore Selector missmatches"
	self assert: MCMockClassE methods size equals: 1.

	"forced methods cannot be executed, because no return (pharo language is weird)"
	snippet isScripting ifFalse: [ self testExecute: MCMockClassE >> definition selector ].

	definition unload.
	self assert: MCMockClassE methods size equals: 0
]

{ #category : 'tests' }
RBCodeSnippetTest >> testParse [

	| ast |
	ast := snippet parse.

	self
		assert: ast isFaulty
		equals: (snippet isParseFaulty ifNil: [ snippet isFaulty ]).

	"Smoke test on the method node"
	self assert: ast methodNode reformatSource isString.

	snippet nodePositions ifNotNil: [
		self assert: ast asPositionDebugString equals: snippet nodePositions ].

	"Smoke test on each AST node (in alphabetic order of selectors)"
	ast nodesDo: [ :node |
		self assert: ((node allParents includes: ast) or: [node = ast]).
		node start to: node stop do: [ :i | self assert: ((node bestNodeFor: (i to: i)) isKindOf: ASTProgramNode) ].
		node start+1 to: node stop do: [ :i | self assert: ((node bestNodeForPosition: i) isKindOf: ASTProgramNode) ].
		self assert: node dump equals: node dump.
		node hasMultipleReturns.
		node hasNonLocalReturn.
		self assert: node sourceCode isString.
		node start to: node stop do: [ :i | self assert: ((node nodeForOffset: i) isKindOf: ASTProgramNode) ].
		self assert: node printString isString.
		self assert: node selfMessages isCollection.
		self assert: node methodNode = ast equals: snippet isScripting not.
		self assert: (node methodOrBlockNode isKindOf: ASTProgramNode).
	]
]

{ #category : 'tests' }
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

{ #category : 'tests' }
RBCodeSnippetTest >> testSimpleFormattedCode [

	| ast fast east |
	ast := snippet parse.
	fast := (RBSimpleFormatter format: ast) reject: #isSeparator.
	east := snippet formattedCode reject: #isSeparator.
	self assert: fast  equals: east
]
