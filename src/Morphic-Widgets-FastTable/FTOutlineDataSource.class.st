"
I'm a data source which will have many list of elements. My goal is to allow a FTOutlineTable to render me with separators between my differents items.

Users might want to subclass me to have better options.
"
Class {
	#name : #FTOutlineDataSource,
	#superclass : #FTDataSource,
	#instVars : [
		'items'
	],
	#category : #'Morphic-Widgets-FastTable-DataSource'
}

{ #category : #'instance creation' }
FTOutlineDataSource class >> items: aCollectionOfCollection [
	" I take a collection of collection of elements."

	^ self new
		items: aCollectionOfCollection;
		yourself
]

{ #category : #accessing }
FTOutlineDataSource >> cellColumn: column row: rowIndex [
	| cell |
	cell := FTCellMorph new
		addMorphBack: (self elementAt: rowIndex) asMorph;
		yourself.
	(self needSeparatorBefore: rowIndex)
		ifTrue: [ cell withTopSeparator ].
	^ cell
]

{ #category : #accessing }
FTOutlineDataSource >> elementAt: index [
	| tmpIndex |
	tmpIndex := index.
	self items
		do: [ :elements | 
			tmpIndex <= elements size
				ifTrue: [ ^ elements at: tmpIndex ]
				ifFalse: [ tmpIndex := tmpIndex - elements size ] ].
	self errorSubscriptBounds: index
]

{ #category : #accessing }
FTOutlineDataSource >> headerColumn: column [
	^ self basicHeaderCellFor: column
]

{ #category : #accessing }
FTOutlineDataSource >> items [
	^ items
]

{ #category : #accessing }
FTOutlineDataSource >> items: anObject [
	items := anObject
]

{ #category : #testing }
FTOutlineDataSource >> needSeparatorBefore: index [
	| tmpIndex |
	index <= 1
		ifTrue: [ ^ false ].
	tmpIndex := index.
	self items
		do: [ :elements | 
			elements size >= tmpIndex
				ifTrue: [ ^ tmpIndex = 1 ]
				ifFalse: [ tmpIndex := tmpIndex - elements size ] ].
	self errorSubscriptBounds: index
]

{ #category : #accessing }
FTOutlineDataSource >> newDataSourceMatching: aFTFilter [
	| newItems |
	newItems := items class withAll: (items collect: [ :coll | coll select: [ :each | aFTFilter matches: (self toString: each) ] ]).
	^ self class new items: newItems
]

{ #category : #accessing }
FTOutlineDataSource >> numberOfRows [
	^ self items
		ifNotNil: [ | cpt |
			cpt := 0.
			self items do: [ :elements | cpt := cpt + elements size ].
			cpt ]
		ifNil: [ 0 ]
]

{ #category : #accessing }
FTOutlineDataSource >> searchText: aString [
	| index result |
	aString isEmptyOrNil
		ifTrue: [ ^ #() ].
	index := 0.
	result := OrderedCollection new.
	self items
		do: [ :elements | 
			elements
				do: [ :each | 
					index := index + 1.
					((self toString: each) beginsWith: aString)
						ifTrue: [ result add: index ] ] ].
	^ result asArray
]
