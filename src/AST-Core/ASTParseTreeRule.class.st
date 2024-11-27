"
ASTParseTreeRule is the abstract superclass of all of the parse tree searching rules. A parse tree rule is the first-class representation of a particular rule to search for. The owner of a rule is the algorithm that actually executes the search. This arrangement allows multiple searches to be conducted by a single Searcher.

Instance Variables:
	owner	<ParseTreeSearcher>	The searcher that is actually performing the search.
	searchTree	<ASTProgramNode>	The parse tree to be searched.


"
Class {
	#name : 'ASTParseTreeRule',
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
ASTParseTreeRule class >> isAbstract [

	^ self == ASTParseTreeRule
]

{ #category : 'instance creation' }
ASTParseTreeRule class >> methodSearch: aString [
	^(self new)
		methodSearchString: aString;
		yourself
]

{ #category : 'instance creation' }
ASTParseTreeRule class >> search: aString [
	^(self new)
		searchString: aString;
		yourself
]

{ #category : 'testing' }
ASTParseTreeRule >> canMatch: aProgramNode [
	^true
]

{ #category : 'private' }
ASTParseTreeRule >> context [
	^owner context
]

{ #category : 'matching' }
ASTParseTreeRule >> foundMatchFor: aProgramNode [
	^aProgramNode
]

{ #category : 'initialization' }
ASTParseTreeRule >> methodSearchString: aString [
	searchTree := self parserClass parseRewriteMethod: aString
]

{ #category : 'initialization' }
ASTParseTreeRule >> owner: aParseTreeSearcher [
	owner := aParseTreeSearcher
]

{ #category : 'initialization' }
ASTParseTreeRule >> parserClass [
	^ RBParser
]

{ #category : 'matching' }
ASTParseTreeRule >> performOn: aProgramNode [
	self context empty.
	^((searchTree match: aProgramNode inContext: self context)
		and: [self canMatch: aProgramNode])
			ifTrue:
				[owner recusivelySearchInContext.
				self foundMatchFor: aProgramNode]
			ifFalse: [nil]
]

{ #category : 'initialization' }
ASTParseTreeRule >> searchString: aString [
	searchTree := self parserClass parseRewriteExpression: aString
]

{ #category : 'accessing' }
ASTParseTreeRule >> sentMessages [
	^searchTree sentMessages
]
