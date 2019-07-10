"
Example for a paginated class list
"
Class {
	#name : #SimplestPaginatedClassListExample,
	#superclass : #PaginatedMorphTreeModel,
	#instVars : [
		'rootClass'
	],
	#category : #'Morphic-Examples-MorphTreeWidget'
}

{ #category : #examples }
SimplestPaginatedClassListExample class >> example [
	<sampleInstance>

	^SimplestPaginatedClassListExample new openOn: Object
]

{ #category : #view }
SimplestPaginatedClassListExample >> defaultTreeMorph [
	^ super defaultTreeMorph
		multiSelection: true;
		autoMultiSelection: true;
		yourself
]

{ #category : #accessing }
SimplestPaginatedClassListExample >> itemsForTest [
	^ rootClass allSubclasses
]

{ #category : #view }
SimplestPaginatedClassListExample >> openDialogOn: aClass [ 
	rootClass := aClass.
	^ self openDialogWindowIn: self currentWorld title: 'All Object subclasses'

]

{ #category : #'instance creation' }
SimplestPaginatedClassListExample >> openOn: aClass [ 
	rootClass := aClass.
	(self dialogWindowIn: self currentWorld title: 'All Object subclasses') openInWorld

]

{ #category : #accessing }
SimplestPaginatedClassListExample >> rootItems [
	^ self itemsForTest asArray sort: [:a :b | a name < b name]
]
