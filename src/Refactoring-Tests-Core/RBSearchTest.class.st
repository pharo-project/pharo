Class {
	#name : #RBSearchTest,
	#superclass : #TestCase,
	#instVars : [
		'classSearches',
		'currentSelector'
	],
	#category : #'Refactoring-Tests-Core'
}

{ #category : #accessing }
RBSearchTest >> buildArgumentSearch [
	self 
		createArgumentSearchWith: 'aSmalllintContext'
		selectors: #(#checkMethod: #checkClass: )
		inClass: RBBasicLintRuleTestData
]

{ #category : #accessing }
RBSearchTest >> buildMessageSearch [
	self 
		createSearchWith: '``@receiver -> ``@arg'
		selectors: #(#superSends #superSends )
		inClass: RBTransformationRuleTestData
]

{ #category : #accessing }
RBSearchTest >> buildMethodArgumentSearch [
	"Two argument methods"
	self 
		createMethodSearchWith: '`arg1: `arg1 `arg2: `arg2 | `@temps | `@.Stmts'
		selectors: #(
				#subclassOf:overrides:
				#createMatcherFor:method:
				#createParseTreeRule:name:
			)
		inClass: RBBasicLintRuleTestData class
]

{ #category : #accessing }
RBSearchTest >> buildMethodSearch [
	"Simple forwarders"
	self 
		createMethodSearchWith: '`@methodName: `@args ^`@object `@methodName: `@args'
		selectors: #(#problemCount #isEmpty )
		inClass: RBTransformationRuleTestData
]

{ #category : #accessing }
RBSearchTest >> buildMethodTitleSearch [
	self 
		createMethodSearchWith: 'initialize | `@temps | `@.Stmts'
		selectors: #(#initialize )
		inClass: RBBasicLintRuleTestData
]

{ #category : #accessing }
RBSearchTest >> buildSimpleLiteralSearch [
	| search |
	search := self parseTreeSearcher.
	search
		matchesAnyTreeOf:
			(Array
				with:
					(RBParser
						parseExpression:
							'#(''bugs'' ''possible bugs'' ''unnecessary code'' ''intention revealing'' ''miscellaneous'')'))
		do: [ :aNode :answer | 
			answer
				remove: self currentSelector ifAbsent: [ self error: 'failed' ];
				yourself ].
	search answer: #(#protocolsToCheck) asBag.
	(classSearches
		at: RBBasicLintRuleTestData class
		ifAbsentPut: [ Set new ]) add: search
]

{ #category : #accessing }
RBSearchTest >> buildSimpleVariableSearch [
	self 
		createSearchWith: 'result'
		selectors: #(
				#checkClass:
				#checkMethod:
				#isEmpty
				#problemCount
				#resetResult
				#resetResult
				#resetResult
				#result
				#result:
				#resultClass:
				#newResultClass:
				#viewResults
			)
		inClass: RBBasicLintRuleTestData
]

{ #category : #accessing }
RBSearchTest >> buildStatementSearch [
	"Two or more statements"
	self 
		createSearchWith: '| `@temps | ``@.Stmts1. ``.Stmt1. ``@.Stmts2. ``.Stmt2. ``@.Stmts3'
		selectors: #(
				#checkMethod:
				#rewriteUsing:
				#viewResults
				#superSends
			)
		inClass: RBTransformationRuleTestData
]

{ #category : #accessing }
RBSearchTest >> createArgumentSearchWith: aCodeString selectors: selectorCollection inClass: aClass [
	| search |
	search := self parseTreeSearcher.
	search
		matchesAnyArgumentOf: (Array with: aCodeString)
		do: [ :aNode :answer | 
			answer
				remove: self currentSelector ifAbsent: [ self error: 'failed' ];
				yourself ].
	search answer: selectorCollection asBag.
	(classSearches at: aClass ifAbsentPut: [ Set new ]) add: search
]

{ #category : #accessing }
RBSearchTest >> createMethodSearchWith: aCodeString selectors: selectorCollection inClass: aClass [
	| search |
	search := self parseTreeSearcher.
	search
		matchesAnyMethodOf: (Array with: aCodeString)
		do: [ :aNode :answer | 
			answer
				remove: self currentSelector ifAbsent: [ self error: 'failed' ];
				yourself ].
	search answer: selectorCollection asBag.
	(classSearches at: aClass ifAbsentPut: [ Set new ]) add: search
]

{ #category : #accessing }
RBSearchTest >> createSearchWith: aCodeString selectors: selectorCollection inClass: aClass [
	| search |
	search := self parseTreeSearcher.
	search
		matches: aCodeString
		do: [ :aNode :answer | 
			answer
				remove: self currentSelector ifAbsent: [ self error: 'failed' ];
				yourself ].
	search answer: selectorCollection asBag.
	(classSearches at: aClass ifAbsentPut: [ Set new ]) add: search
]

{ #category : #accessing }
RBSearchTest >> currentSelector [
	^ currentSelector
]

{ #category : #accessing }
RBSearchTest >> parseTreeSearcher [
	^ RBParseTreeSearcher new
]

{ #category : #running }
RBSearchTest >> setUp [
	super setUp.
	classSearches := Dictionary new.
	self
		buildMethodSearch;
		buildSimpleVariableSearch;
		buildSimpleLiteralSearch;
		buildMessageSearch;
		buildStatementSearch;
		buildArgumentSearch;
		buildMethodTitleSearch;
		buildMethodArgumentSearch
]

{ #category : #tests }
RBSearchTest >> testAllSearches [
	classSearches
		keysAndValuesDo: [ :class :searches | 
			class selectors
				do: [ :sel | 
					currentSelector := sel.
					searches do: [ :each | each executeTree: (class parseTreeFor: sel) initialAnswer: each answer ] ] ].
	classSearches do: [ :searches | searches do: [ :each | self assertEmpty: each answer ] ]
]
