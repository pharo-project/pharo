Class {
	#name : #SpListFastTableDataSource,
	#superclass : #FTDataSource,
	#instVars : [
		'model'
	],
	#category : #'Spec2-Adapters-Morphic-List'
}

{ #category : #accessing }
SpListFastTableDataSource >> cellColumn: column row: rowIndex [
	| displayValue cell item |
	item := self model itemAt: rowIndex.
	displayValue := column readObject: item.
	cell := FTCellMorph new
		cellInset: 5;
		yourself.
	(self model iconFor: item) ifNotNil: [ :icon | cell addMorphBack: icon asMorph ].
	cell addMorphBack: displayValue asMorph asReadOnlyMorph.
	^ cell
]

{ #category : #accessing }
SpListFastTableDataSource >> elementAt: rowIndex [

	^ self listModel at: rowIndex
]

{ #category : #accessing }
SpListFastTableDataSource >> elements [

	^ self model items
]

{ #category : #accessing }
SpListFastTableDataSource >> headerColumn: column [
	column id ifNil: [ ^ nil ]. 
	^ FTCellMorph new 
		listCentering: #center;
		addMorph: column id asMorph asReadOnlyMorph;
		yourself
]

{ #category : #accessing }
SpListFastTableDataSource >> listModel [
	^ model model
]

{ #category : #accessing }
SpListFastTableDataSource >> menuColumn: column row: rowIndex [
	| menuPresenter |

	menuPresenter := self model contextMenu.
	menuPresenter ifNil: [ ^ nil ].
	^ SpBindings
		value: self model application adapterBindings
		during: [ 
			| m |
			m := menuPresenter value.
			m isMorph 
				ifTrue: [ m ]
				ifFalse: [ m buildWithSpec ] ]
]

{ #category : #accessing }
SpListFastTableDataSource >> model [
	^ model
]

{ #category : #accessing }
SpListFastTableDataSource >> model: anObject [
	model := anObject
]

{ #category : #accessing }
SpListFastTableDataSource >> newDataSourceMatching: aFTFilter [
	| newElements wrappedItem text newDataSource modelCopy |
	
	newElements := self elements select: [ :each | 
		wrappedItem := self model displayValueOf: each.
		table columns anySatisfy: [ :column | 
			text := column transform: wrappedItem.
			aFTFilter matches: text]].
	
	newDataSource := self copy.
	modelCopy := self model copy.
	modelCopy prepareForFilteredDataSourceWith: newElements.
	newDataSource model: modelCopy.
	
	^newDataSource
]

{ #category : #accessing }
SpListFastTableDataSource >> numberOfRows [
	^ model
		ifNil: [ 0 ]
		ifNotNil: [ self listModel size ]
]

{ #category : #accessing }
SpListFastTableDataSource >> searchText: aString [
	| search text result |
	aString isEmptyOrNil ifTrue: [ ^ #() ].
	result := OrderedCollection new.
	search := aString trimBoth asLowercase.
	1 to: self numberOfRows do: [ :rowIndex | 
		text := (self model displayValueAt: rowIndex) contents trimBoth asLowercase.
		(text beginsWith: search)
			ifTrue: [ result add: rowIndex ] ].
	^ result asArray
]

{ #category : #'drag and drop' }
SpListFastTableDataSource >> transferFor: passenger from: aMorph [
    ^(self model transferFor: passenger from: self table) buildWithSpec
]
