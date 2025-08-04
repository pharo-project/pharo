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

{ #category : 'tests' }
OCCodeSnippetTest >> testCodeImporter [

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
		self should: [ importer evaluate ] raise: OCCodeError.
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
OCCodeSnippetTest >> testFormattedCode [

	| ast |
	ast := snippet parse.
	self assert: ast formattedCode withSeparatorsCompacted equals: snippet formattedCode withSeparatorsCompacted
]

{ #category : 'tests' }
OCCodeSnippetTest >> testParse [

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
		node start to: node stop do: [ :i | self assert: ((node bestNodeFor: (i to: i)) isKindOf: OCProgramNode) ].
		node start+1 to: node stop do: [ :i | self assert: ((node bestNodeForPosition: i) isKindOf: OCProgramNode) ].
		self assert: node dump equals: node dump.
		node hasMultipleReturns.
		node hasNonLocalReturn.
		self assert: node sourceCode isString.
		node start to: node stop do: [ :i | self assert: ((node nodeForOffset: i) isKindOf: OCProgramNode) ].
		self assert: node printString isString.
		self assert: node selfMessages isCollection.
		self assert: node methodNode = ast equals: snippet isScripting not.
		self assert: (node methodOrBlockNode isKindOf: OCProgramNode).
	]
]

{ #category : 'tests' }
OCCodeSnippetTest >> testParseOnError [

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
OCCodeSnippetTest >> testSimpleFormattedCode [

	| ast fast east |
	ast := snippet parse.
	fast := (OCSimpleFormatter format: ast) reject: #isSeparator.
	east := snippet formattedCode reject: #isSeparator.
	self assert: fast  equals: east
]
