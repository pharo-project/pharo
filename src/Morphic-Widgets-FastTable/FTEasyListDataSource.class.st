"
I'm a datasource who works with FTEasyListMorph.
I receive blocks to provide my contents. 
"
Class {
	#name : #FTEasyListDataSource,
	#superclass : #FTSimpleDataSource,
	#instVars : [
		'displayBlock',
		'iconBlock',
		'menuBlock'
	],
	#category : #'Morphic-Widgets-FastTable-Easy'
}

{ #category : #accessing }
FTEasyListDataSource >> cellColumn: column row: rowIndex [ 
	^ FTCellMorph new
		cellInset: 5;
		in: [ :me | 
			(self iconFor: rowIndex) 
				ifNotNil: [ :icon | me addMorphBack: icon asMorph ] ];
		addMorphBack: (self displayFor: rowIndex) asMorph;
		yourself
]

{ #category : #accessing }
FTEasyListDataSource >> display: aBlock [
	displayBlock := aBlock
]

{ #category : #private }
FTEasyListDataSource >> displayFor: rowIndex [
	| element |
	element := self elementAt: rowIndex.
	^ displayBlock 
		ifNotNil: [ displayBlock value: element ]
		ifNil: [ ^ element ].
]

{ #category : #accessing }
FTEasyListDataSource >> icon: aBlock [
	iconBlock := aBlock
]

{ #category : #private }
FTEasyListDataSource >> iconFor: rowIndex [
	iconBlock ifNil: [ ^ nil ].
	^ iconBlock value: (self elementAt: rowIndex)
]

{ #category : #accessing }
FTEasyListDataSource >> menu: aBlock [
	menuBlock := aBlock
]

{ #category : #accessing }
FTEasyListDataSource >> menuColumn: column row: rowIndex [
	menuBlock ifNil: [ ^ nil ].
	rowIndex = 0 ifTrue: [ 
		"I could answer a menu without selection, but in this case I will just answer nil, 
		 which means 'no menu'"
		^ nil ].
	^ menuBlock value: (self elementAt: rowIndex)
]
