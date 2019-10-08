"
I am an abstract visitor for the results tree of DrTests.
"
Class {
	#name : #DTResultsTreeVisitor,
	#superclass : #Object,
	#category : #'DrTests-Visitors'
}

{ #category : #visiting }
DTResultsTreeVisitor >> visit: anObject [
	^ anObject acceptVisitor: self
]

{ #category : #visiting }
DTResultsTreeVisitor >> visitDTTreeLeaf: aDTTreeLeaf [
	^ self subclassResponsibility
]

{ #category : #visiting }
DTResultsTreeVisitor >> visitDTTreeNode: aDTTreeNode [
	^ aDTTreeNode subResults collect: [ :subResult | self visit: subResult ]
]
