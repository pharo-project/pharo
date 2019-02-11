"
I define a strategy to handle selections. 
The sense of this class is shown in my children: I allow to handle the case of multiple and simple selection in a well designed way, instead using ifs everywhere.
"
Class {
	#name : #FTSelectionStrategy,
	#superclass : #Object,
	#instVars : [
		'table'
	],
	#category : #'Morphic-Widgets-FastTable-Selection'
}

{ #category : #'instance creation' }
FTSelectionStrategy class >> table: aFastTableMorph [
	^ self new
		table: aFastTableMorph;
		yourself
]

{ #category : #testing }
FTSelectionStrategy >> isMultiple [
	^ self subclassResponsibility
]

{ #category : #testing }
FTSelectionStrategy >> isSimple [
	^ self isMultiple not
]

{ #category : #accessing }
FTSelectionStrategy >> selectIndex: rowIndex event: event [
	self subclassResponsibility
]

{ #category : #accessing }
FTSelectionStrategy >> selectRowIndex: rowIndex event: event [
	self deprecated: 'Use #selectIndex:event: instead.' transformWith: '`@receiver selectRowIndex: `@statements1 event: `@statements2' -> '`@receiver selectIndex: `@statements1 event: `@statements2'.
	self selectIndex: rowIndex event: event
]

{ #category : #accessing }
FTSelectionStrategy >> selectionModeStrategy [
	^ self table selectionModeStrategy
]

{ #category : #accessing }
FTSelectionStrategy >> table [
	^ table
]

{ #category : #accessing }
FTSelectionStrategy >> table: aTable [
	table := aTable
]

{ #category : #private }
FTSelectionStrategy >> toggleIndex: index [
	((self table selectedIndexes includes: index) and: [ self table selectedIndexes size = 1 and: [ self table allowsDeselection ]	"just one selected" ])
		ifTrue: [ self table deselectAll ]
		ifFalse: [ self table selectIndex: index ]
]

{ #category : #private }
FTSelectionStrategy >> toggleRowIndex: index [
	self deprecated: 'Use #toggleIndex: instead.' transformWith: '`@receiver toggleRowIndex: `@statements' -> '`@receiver toggleIndex: `@statements'.
	self toggleIndex: index
]
