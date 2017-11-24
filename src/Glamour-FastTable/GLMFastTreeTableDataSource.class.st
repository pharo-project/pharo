"
I'm a model datasource to add columns support to a tree presentation.

Description 
--------------------

My superclass models a tree. I override  #cellColumn:row: to return a row that contains multiple columns.

I work with a GLMFastTreeTablePresentation and a GLMMorphicFastTreeWithColumnsRenderer.  I rely on TGLMFastTableColumnsRenderer for the logic that actually builds the row morphs. 

Internal Representation and Key Implementation Points.
--------------------

- #rowMorphElementsForFirstColumn:item:in:    I configure the first column with the propper indentation and buttons for expanding collapsing the node.
- #dataFromPresentationItem:                                     I return the data from the given item

    Instance Variables
	columnToSortBy		This is the column to use for sorting the displayed elements


"
Class {
	#name : #GLMFastTreeTableDataSource,
	#superclass : #GLMFastTreeDataSource,
	#traits : 'TGLMFastTableColumnsRenderer',
	#classTraits : 'TGLMFastTableColumnsRenderer classTrait',
	#instVars : [
		'columnToSortBy'
	],
	#category : #'Glamour-FastTable-Model'
}

{ #category : #accessing }
GLMFastTreeTableDataSource >> actionColumn: column [
	"If columns has actions associated, execute them"
	column execute.
	self tableRefresh
]

{ #category : #accessing }
GLMFastTreeTableDataSource >> dataFromPresentationItem: anItem [
	^ anItem data
]

{ #category : #accessing }
GLMFastTreeTableDataSource >> headerColumn: column [
	column id ifNil: [ ^ nil ].
	^ SimpleButtonMorph new
		layoutPolicy: RowLayout new; 
		listDirection: #leftToRight; 
		wrapCentering: #center;
		cellInset: 5@0;
		layoutInset: 5@0;
		label: column id;
		target: self;
		actionSelector: #actionColumn:;
		arguments: {column};
		yourself
]

{ #category : #accessing }
GLMFastTreeTableDataSource >> rowMorphElementsForFirstColumn: aColumn item: anItem in: rowElements [

	anItem depth timesRepeat: [ rowElements add: self class emptyMorph ].
	rowElements add: (self buttonFor: anItem).
	(self iconFor: anItem data) ifNotNil: [ :icon | rowElements add: icon asMorph ].
]

{ #category : #sorting }
GLMFastTreeTableDataSource >> sortBlock [
	^  columnToSortBy sortBlock ifNil: [ |defaultSortBlock|
			defaultSortBlock := columnToSortBy defaultSortBlockForPresentation: self glamourPresentation.
		 	[ :x :y | defaultSortBlock value: x data value: y data ] ]
]

{ #category : #sorting }
GLMFastTreeTableDataSource >> sortByColumn: aGlamourColumn [
	columnToSortBy := (columnToSortBy == aGlamourColumn 
		ifTrue: [ nil ]
		ifFalse: [ aGlamourColumn ]).
		
	columnToSortBy 
		ifNotNil: [ self rootItem children: (self rootItem children sorted: self sortBlock) ]
		ifNil: [ self rootItem data: self basicRoots ].

	self glamourPresentation changed.
]
