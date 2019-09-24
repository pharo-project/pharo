"
I am a visitor that collect the leaves of the DTTreeNode I visit.

If there are multiple levels of DTTreeNode, no problem, I traverse them until I find the leaves.
"
Class {
	#name : #DTLeavesCollector,
	#superclass : #DTResultsTreeVisitor,
	#instVars : [
		'leaves'
	],
	#category : #'DrTests-Visitors'
}

{ #category : #'as yet unclassified' }
DTLeavesCollector class >> collectLeavesOf: aDTTreeNode [
	^ self new
		visit: aDTTreeNode;
		leaves
]

{ #category : #initialization }
DTLeavesCollector >> initialize [
	super initialize.
	leaves := OrderedCollection new.
]

{ #category : #accessing }
DTLeavesCollector >> leaves [
	^ leaves
]

{ #category : #accessing }
DTLeavesCollector >> leaves: anObject [
	leaves := anObject
]

{ #category : #visiting }
DTLeavesCollector >> visitDTTreeLeaf: aDTTreeLeaf [
	self leaves add: aDTTreeLeaf
]
