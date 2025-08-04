"
ASTParseTreeRule is the abstract superclass of all of the parse tree searching rules. A parse tree rule is the first-class representation of a particular rule to search for. The owner of a rule is the algorithm that actually executes the search. This arrangement allows multiple searches to be conducted by a single Searcher.

Instance Variables:
	owner	<ParseTreeSearcher>	The searcher that is actually performing the search.
	searchTree	<ASTProgramNode>	The parse tree to be searched.


"
Class {
	#name : 'OCParseTreeRule',
	#superclass : 'Object',
	#instVars : [
		'searchTree',
		'owner'
	],
	#category : 'AST-Core-Matching',
	#package : 'AST-Core',
	#tag : 'Matching'
}

{ #category : 'testing' }
OCParseTreeRule class >> isAbstract [

	^ self == OCParseTreeRule
]

{ #category : 'instance creation' }
OCParseTreeRule class >> methodSearch: aString [
	^(self new)
		methodSearchString: aString;
		yourself
]

{ #category : 'instance creation' }
OCParseTreeRule class >> search: aString [
	^(self new)
		searchString: aString;
		yourself
]

{ #category : 'testing' }
OCParseTreeRule >> canMatch: aProgramNode [
	^true
]

{ #category : 'private' }
OCParseTreeRule >> context [
	^owner context
]

{ #category : 'matching' }
OCParseTreeRule >> foundMatchFor: aProgramNode [
	^aProgramNode
]

{ #category : 'initialization' }
OCParseTreeRule >> methodSearchString: aString [
	searchTree := self parserClass parseRewriteMethod: aString
]

{ #category : 'initialization' }
OCParseTreeRule >> owner: aParseTreeSearcher [
	owner := aParseTreeSearcher
]

{ #category : 'initialization' }
OCParseTreeRule >> parserClass [
	^ OCParser
]

{ #category : 'matching' }
OCParseTreeRule >> performOn: aProgramNode [
	self context empty.
	^((searchTree match: aProgramNode inContext: self context)
		and: [self canMatch: aProgramNode])
			ifTrue:
				[owner recusivelySearchInContext.
				self foundMatchFor: aProgramNode]
			ifFalse: [nil]
]

{ #category : 'initialization' }
OCParseTreeRule >> searchString: aString [
	searchTree := self parserClass parseRewriteExpression: aString
]

{ #category : 'accessing' }
OCParseTreeRule >> sentMessages [
	^searchTree sentMessages
]
