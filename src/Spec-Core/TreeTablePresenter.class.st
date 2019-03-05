"
A TreeTablePresenter handles a tree table.
Tables defines columns (see #addColumn: and TableColumn hierarchy).
Tables receive items (see #items:) to feed the table.

self example

"
Class {
	#name : #TreeTablePresenter,
	#superclass : #TableBasePresenter,
	#instVars : [
		'rootsHolder',
		'childrenBlock'
	],
	#category : #'Spec-Core-Widgets-Table'
}

{ #category : #specs }
TreeTablePresenter class >> adapterName [

	^ #TreeTableAdapter
]

{ #category : #example }
TreeTablePresenter class >> example [
	<sampleInstance>

	^ self new
		addColumn: (CompositeTableColumn new
			title: 'Classes';
			addColumn: (ImageTableColumn new 
				width: 20;
				evaluated: #systemIcon;
				yourself);
			addColumn: (StringTableColumn new 
				evaluated: #name;
				yourself);
			yourself);
		roots: { Object };
		children: [ :aClass | aClass subclasses ];
		openWithSpec
]

{ #category : #private }
TreeTablePresenter >> children [

	^ childrenBlock ifNil: [ [ :item | nil ] ]
]

{ #category : #accessing }
TreeTablePresenter >> children: aBlock [

	childrenBlock := aBlock
]

{ #category : #accessing }
TreeTablePresenter >> childrenFor: anObject [ 

	^ self children value: anObject
]

{ #category : #initialization }
TreeTablePresenter >> initialize [

	super initialize.
	rootsHolder := #() asValueHolder.
	rootsHolder whenChangedDo: [ self rootsChanged ]
]

{ #category : #private }
TreeTablePresenter >> itemAt: index of: anArray then: path [
	"dives into elements of tree to find the one that corresponds to path"
	| element |
	
	element := anArray at: index.
	^ path 
		ifNotEmpty: [ 
			self 
				itemAt: path first 
				of: (self childrenFor: element) 
				then: path allButFirst ]
		ifEmpty: [ 
			element ]
]

{ #category : #accessing }
TreeTablePresenter >> itemAtPath: anArray [

	^ self 
		itemAt: anArray first
		of: self roots
		then: anArray allButFirst
]

{ #category : #accessing }
TreeTablePresenter >> roots [

	^ rootsHolder value
]

{ #category : #accessing }
TreeTablePresenter >> roots: aCollection [

	rootsHolder value: aCollection
]

{ #category : #private }
TreeTablePresenter >> rootsChanged [

	self changed: #items
]

{ #category : #api }
TreeTablePresenter >> selectedPath: path [

	selectionHolder index value: path.
	selectionHolder selection value: (self itemAtPath: path).
]
