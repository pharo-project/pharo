"
I build a resultset with all possible candidates for an AST node, pre-sorted with a sorter.
"
Class {
	#name : 'CoGlobalSorterResultSetBuilder',
	#superclass : 'CoASTResultSetBuilder',
	#instVars : [
		'sorterClass'
	],
	#category : 'HeuristicCompletion-Model-Old-Unused',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Old-Unused'
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

	| fetcher entries |
	entries := aNode completionEntries: completionContext position.
	"we should probably be able to remove all the implementations of completionEntries:"
	fetcher := CoCollectionFetcher onCollection: entries.

	^ self configureFetcher: fetcher forNode: aNode
]
