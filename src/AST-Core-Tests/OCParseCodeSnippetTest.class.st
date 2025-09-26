Class {
	#name : 'OCParseCodeSnippetTest',
	#superclass : 'OCCodeSnippetTest',
	#category : 'AST-Core-Tests-Snippets',
	#package : 'AST-Core-Tests',
	#tag : 'Snippets'
}

{ #category : 'tests' }
OCParseCodeSnippetTest >> testFormattedCode [

	| ast |
	ast := snippet parse.
	self assert: ast formattedCode withSeparatorsCompacted equals: snippet formattedCode withSeparatorsCompacted
]

{ #category : 'tests' }
OCParseCodeSnippetTest >> testParse [

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
OCParseCodeSnippetTest >> testParseOnError [

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
OCParseCodeSnippetTest >> testSimpleFormattedCode [

	| ast fast east |
	ast := snippet parse.
	fast := (OCSimpleFormatter format: ast) reject: #isSeparator.
	east := snippet formattedCode reject: #isSeparator.
	self assert: fast  equals: east
]
