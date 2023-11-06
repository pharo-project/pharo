"
I build a resultset with all possible candidates for an AST node, pre-sorted with a sorter.
"
Class {
	#name : 'CoGlobalSorterResultSetBuilder',
	#superclass : 'CoASTResultSetBuilder',
	#instVars : [
		'sorterClass'
	],
	#category : 'HeuristicCompletion-Model-SystemIntegration',
	#package : 'HeuristicCompletion-Model',
	#tag : 'SystemIntegration'
}

{ #category : 'API - building' }
CoGlobalSorterResultSetBuilder >> parseNode [

	| aNewNode |
	aNewNode := super parseNode.
	TypingVisitor new visitNode: aNewNode methodNode.
	^ aNewNode
]

{ #category : 'accessing' }
CoGlobalSorterResultSetBuilder >> sorterClass [

	^ sorterClass ifNil: [ completionContext class sorterClass ]
]

{ #category : 'accessing' }
CoGlobalSorterResultSetBuilder >> sorterClass: anObject [
	sorterClass := anObject
]

{ #category : 'visiting' }
CoGlobalSorterResultSetBuilder >> visitNode: aNode [

	| fetcher entries sorter |
	entries := aNode completionEntries: completionContext position.
	sorter := self sorterClass new context: completionContext.
	entries := sorter sortCompletionList: entries.

	fetcher := CoCollectionFetcher onCollection: entries.

	^ self configureFetcher: fetcher forNode: aNode
]
