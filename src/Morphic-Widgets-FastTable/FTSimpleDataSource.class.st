"
I'm a simple datasource and just implement a direct-to-table provision of my elements.  I can work with the default implementation of FTTableMorph. 

Users might want to subclass me to have better options.
"
Class {
	#name : #FTSimpleDataSource,
	#superclass : #FTDataSource,
	#instVars : [
		'elements',
		'unsortedElements'
	],
	#category : #'Morphic-Widgets-FastTable-DataSource'
}

{ #category : #'instance creation' }
FTSimpleDataSource class >> elements: aCollection [
	^ self new elements: aCollection
]

{ #category : #accessing }
FTSimpleDataSource >> cellColumn: column row: rowIndex [ 
	^ FTCellMorph new
		addMorphBack: (self elementAt: rowIndex) asMorph;
		yourself
	
	
]

{ #category : #accessing }
FTSimpleDataSource >> elementAt: index [
	^ self elements at: index
]

{ #category : #accessing }
FTSimpleDataSource >> elements [
	^ elements
]

{ #category : #accessing }
FTSimpleDataSource >> elements: anObject [
	elements := anObject
]

{ #category : #accessing }
FTSimpleDataSource >> headerColumn: column [
	^ self basicHeaderCellFor: column
]

{ #category : #accessing }
FTSimpleDataSource >> newDataSourceMatching: aFTFilter [
	| newElements |
	newElements := Array withAll: (elements select: [ :each | 
		aFTFilter matches: (self toString: each) ]).
	^ self class new elements: newElements
]

{ #category : #accessing }
FTSimpleDataSource >> numberOfRows [ 
	^ self elements 
		ifNotNil: [ self elements size ]
		ifNil: [ 0 ]
]

{ #category : #accessing }
FTSimpleDataSource >> searchText: aString [
	| result |
	aString isEmptyOrNil ifTrue: [ ^ #() ].
	result := OrderedCollection new.
	self elements withIndexDo: [ :each :index |
		((self toString: each) beginsWith: aString)
			ifTrue: [ result add: index ] ].
	^ result asArray
]

{ #category : #sorting }
FTSimpleDataSource >> sortElements: aSortFunction [
	unsortedElements ifNil: [ unsortedElements := self elements ].
	self elements: (self elements sorted: aSortFunction)
]

{ #category : #sorting }
FTSimpleDataSource >> unsortElements [
	unsortedElements ifNil: [ ^ self ].

	self elements: unsortedElements.
	unsortedElements := nil
]
