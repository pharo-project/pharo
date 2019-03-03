"
I am the adapter used to realise a TablePresenter.
I will create a table (using FTTableMorph), using the columns TableColumn* as definitions.

Is important notice here that most of the row/cell creation happens on MorphicTableDataSource. Check there for some implementation details.
"
Class {
	#name : #MorphicTableAdapter,
	#superclass : #AbstractMorphicAdapter,
	#category : #'Spec-MorphicAdapters-Table'
}

{ #category : #factory }
MorphicTableAdapter >> addModelTo: tableMorph [

	self model columns do: [ :each | 
		tableMorph addColumn: (self newColumnFrom: each) ].
	self ensureAtLeastOneColumnIn: tableMorph.

	self isShowingColumnHeaders 
		ifTrue: [ tableMorph showColumnHeaders ] 
		ifFalse: [ tableMorph hideColumnHeaders ].

	self isResizable 
		ifTrue: [ tableMorph beResizable ] 
		ifFalse: [ tableMorph beNotResizable ].

	tableMorph dataSource: self newDataSource
]

{ #category : #factory }
MorphicTableAdapter >> buildWidget [
	| tableMorph |
	tableMorph := FTTableMorph new
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		yourself.

	self addModelTo: tableMorph.

	^ tableMorph
]

{ #category : #factory }
MorphicTableAdapter >> defaultColumn [

	^ StringTableColumn new evaluated: #yourself
]

{ #category : #factory }
MorphicTableAdapter >> ensureAtLeastOneColumnIn: tableMorph [
	self model columns ifNotEmpty: [ ^ self ].
	tableMorph addColumn: (self newColumnFrom: self defaultColumn)
]

{ #category : #testing }
MorphicTableAdapter >> isResizable [
	^ self model isResizable
]

{ #category : #testing }
MorphicTableAdapter >> isShowingColumnHeaders [
	^ self model isShowingColumnHeaders
]

{ #category : #'private factory' }
MorphicTableAdapter >> newColumnFrom: aTableColumn [

	^ (MorphicTableColumn on: aTableColumn)
		width: aTableColumn width;
		yourself
]

{ #category : #'private factory' }
MorphicTableAdapter >> newDataSource [ 

	^ MorphicTableDataSource new 
		elements: self model items;
		yourself
]
