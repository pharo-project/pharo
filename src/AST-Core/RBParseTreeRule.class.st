"
RBParseTreeRule is the abstract superclass of all of the parse tree searching rules. A parse tree rule is the first class representation of a particular rule to search for. The owner of a rule is the algorithm that actually executes the search. This arrangement allows multiple searches to be conducted by a single Searcher.

Instance Variables:
	owner	<ParseTreeSearcher>	The searcher that is actually performing the search.
	searchTree	<RBProgramNode>	The parse tree to be searched.


"
Class {
	#name : #RBParseTreeRule,
	#superclass : #Object,
	#instVars : [
		'searchTree',
		'owner'
	],
	#category : #'AST-Core-Matching'
}

{ #category : #'instance creation' }
RBParseTreeRule class >> methodSearch: aString [ 
	^(self new)
		methodSearchString: aString;
		yourself
]

{ #category : #'instance creation' }
RBParseTreeRule class >> search: aString [ 
	^(self new)
		searchString: aString;
		yourself
]

{ #category : #testing }
RBParseTreeRule >> canMatch: aProgramNode [ 
	^true
]

{ #category : #private }
RBParseTreeRule >> context [
	^owner context
]

{ #category : #matching }
RBParseTreeRule >> foundMatchFor: aProgramNode [
	^aProgramNode
]

{ #category : #'initialize-release' }
RBParseTreeRule >> methodSearchString: aString [ 
	searchTree := RBParser parseRewriteMethod: aString
]

{ #category : #'initialize-release' }
RBParseTreeRule >> owner: aParseTreeSearcher [
	owner := aParseTreeSearcher
]

{ #category : #matching }
RBParseTreeRule >> performOn: aProgramNode [ 
	self context empty.
	^((searchTree match: aProgramNode inContext: self context) 
		and: [self canMatch: aProgramNode]) 
			ifTrue:
				[owner recusivelySearchInContext.
				self foundMatchFor: aProgramNode]
			ifFalse: [nil]
			
		
]

{ #category : #'initialize-release' }
RBParseTreeRule >> searchString: aString [ 
	searchTree := RBParser parseRewriteExpression: aString
]

{ #category : #accessing }
RBParseTreeRule >> sentMessages [
	^searchTree sentMessages
]
