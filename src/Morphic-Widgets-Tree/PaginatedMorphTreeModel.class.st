"
A morphic based tree model with pagination
"
Class {
	#name : #PaginatedMorphTreeModel,
	#superclass : #MorphTreeModel,
	#instVars : [
		'pageSize'
	],
	#category : #'Morphic-Widgets-Tree-Pagination'
}

{ #category : #examples }
PaginatedMorphTreeModel class >> example [
	"This test demonstrate a paginating morphTreeMorph in action."

	"self example."

	| treeMorph aWindow aModel |
	aModel := self itemsList: (1 to: 100) asArray.
	aWindow := aModel theme newWindowIn: self currentWorld for: aModel title: 'Example'.
	treeMorph := aModel defaultTreeMorph.
	treeMorph pageSize: 30.
	treeMorph buildContents.
	aWindow addMorph: treeMorph fullFrame: LayoutFrame identity.
	aWindow open.
	aModel expandItemPath: {40}.
	self assert: treeMorph pager currentPage == 2
]

{ #category : #'pager managing' }
PaginatedMorphTreeModel >> chunkSize [
	^ pageSize 
]

{ #category : #accessing }
PaginatedMorphTreeModel >> chunkSize: anIntegerOrNil [
	self setPageSize: anIntegerOrNil.
	self announce: (MorphTreeChangeRequest chunkSize: anIntegerOrNil)

]

{ #category : #'pager managing' }
PaginatedMorphTreeModel >> pageSize [
	^ pageSize

]

{ #category : #accessing }
PaginatedMorphTreeModel >> pageSize: anIntegerOrNil [
	self setPageSize: anIntegerOrNil.
	self announce: (MorphTreeChangeRequest pageSize: anIntegerOrNil)

]

{ #category : #accessing }
PaginatedMorphTreeModel >> rootNodes [
	| computedRootNodes |
	computedRootNodes := super rootNodes.
	self defaultChunkSize 
		ifNotNil: [computedRootNodes size > self defaultChunkSize
			ifTrue: [self chunkSize: self defaultChunkSize]]
		ifNil: [
			self defaultPageSize 
				ifNotNil: [computedRootNodes size > self defaultPageSize
					ifTrue: [self pageSize: self defaultPageSize]]].
	^ computedRootNodes

]

{ #category : #'pager managing' }
PaginatedMorphTreeModel >> setPageSize: anIntegerOrNil [
	pageSize := anIntegerOrNil.

]

{ #category : #accessing }
PaginatedMorphTreeModel >> treeMorphClass [
	^ PaginatedMorphTreeMorph 
]
