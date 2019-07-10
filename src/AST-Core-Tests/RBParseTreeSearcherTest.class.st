"
SUnit tests for RBParseTreeSearcher
"
Class {
	#name : #RBParseTreeSearcherTest,
	#superclass : #TestCase,
	#instVars : [
		'searcher'
	],
	#category : #'AST-Core-Tests-Matching'
}

{ #category : #helpers }
RBParseTreeSearcherTest >> parseExpression: aString [ 

	^ RBParser parseExpression: aString
]

{ #category : #'tests - to de refined' }
RBParseTreeSearcherTest >> parseTreeSearcher [
	^ RBParseTreeSearcher new
]

{ #category : #running }
RBParseTreeSearcherTest >> setUp [
	super setUp.
	searcher := self parseTreeSearcher
]

{ #category : #'tests - to de refined' }
RBParseTreeSearcherTest >> testDynamicArrayStatements [
	"match in context for arraynodes wrongly matched all array statements against the last variable, if all variables are pattern-list-variables"

	| dict |
	searcher matches: '{`@first. `@second. `@third.}' do: [ :aNode :answer |  dict:= searcher context ].
	dict := searcher executeTree: (self parseExpression:'{ (1@2) . Color red . 3}').
	self assert: (dict at: (RBPatternVariableNode named:'`@first')) formattedCode equals: '(1 @ 2)'.
	self assert: (dict at: (RBPatternVariableNode named:'`@second')) formattedCode equals: 'Color red'.
	self assert: (dict at: (RBPatternVariableNode named:'`@third')) formattedCode equals: '3'
		
]

{ #category : #'tests - to de refined' }
RBParseTreeSearcherTest >> testDynamicArrayWithStatementListPattern [
	"pattern with statementlist in a dynamic array, to match all dynamic array expressions"

	| dict |
	searcher matches: '{`.@stmts.}' do: [ :aNode :answer |  dict:= searcher context ].
	dict := searcher executeTree: (self parseExpression:'{ (1@2) . Color red . 3}').
	self assert: (dict at: (RBPatternVariableNode named: '`.@stmts')) size equals: 3

]

{ #category : #'tests - to de refined' }
RBParseTreeSearcherTest >> testSearching [
	searcher
		matches: '``@rcv at: ``@arg `sel: ``@arg1'
		do: [ :aNode :answer | answer + 1 ].
	self
		assert:
			(searcher
				executeTree:
					(self
						parseExpression: 'self at: 1 put: 2; at: 2 ifAbsent: []; ifAbsent: 2 at: 1; at: 4; foo')
				initialAnswer: 0)
		equals: 2.
	searcher := self parseTreeSearcher.
	searcher
		matches: '``@rcv `at: ``@arg1 `at: ``@arg2'
		do: [ :aNode :answer | answer + 1 ].
	self
		assert:
			(searcher
				executeTree:
					(RBParser
						parseExpression: 'self at: 1 at: 3; at: 1 put: 32; at: 2; foo; at: 1 at: 1 at: 2')
				initialAnswer: 0)
		equals: 1.
	searcher := self parseTreeSearcher.
	searcher
		matchesMethod: 'at: `object `put: `o1 ``@rcv `put: 1'
		do: [ :aNode :answer | true ].
	self
		assert:
			(searcher
				executeTree: (RBParser parseMethod: 'at: a put: b self foo put: 1')
				initialAnswer: false)
]

{ #category : #tests }
RBParseTreeSearcherTest >> testSearchingComposedSelector [
	
	searcher 
		matches: '``@rcv at: ``@arg `sel: ``@arg1'
		do: [ :aNode :answer | answer + 1].
		
	"the result is 2 because the parse tree searcher only finds at:x: messages like at:put: and at:ifAbsent:"	
	self 
		assert: (searcher 
					executeTree: (self 	
							parseExpression: 'self at: 1 put: 2; at: 2 ifAbsent: []; ifAbsent: 2 at: 1; at: 4; foo')
					initialAnswer: 0) equals: 2
	
]

{ #category : #tests }
RBParseTreeSearcherTest >> testSearchingComposedSelector2 [

	searcher
		matches: '``@rcv at: ``@arg'
		do: [ :aNode :answer | answer + 1 ].
	"the result is 1 because the parse tree searcher only finds at: messages like at: and not ifAbsent: at:"	
	self 
		assert: (searcher 
					executeTree: (self parseExpression: 'self ifAbsent: 2 at: 1; at: 4; foo')
					initialAnswer: 0)
		equals: 1
]
