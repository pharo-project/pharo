Class {
	#name : #SpecListFastTableDataSource,
	#superclass : #FTDataSource,
	#instVars : [
		'model'
	],
	#category : #'Spec-MorphicAdapters-List'
}

{ #category : #accessing }
SpecListFastTableDataSource >> cellColumn: column row: rowIndex [
	| displayValue cell |
	displayValue := column readObject: (self model itemAt: rowIndex).
	cell := FTCellMorph new
		cellInset: 5;
		yourself.
	cell addMorphBack: displayValue asMorph.
	^ cell
]

{ #category : #accessing }
SpecListFastTableDataSource >> elementAt: rowIndex [

	^ self listModel at: rowIndex
]

{ #category : #accessing }
SpecListFastTableDataSource >> elements [

	^ self model items
]

{ #category : #accessing }
SpecListFastTableDataSource >> headerColumn: column [
	column id ifNil: [ ^ nil ]. 
	^ FTCellMorph new 
		listCentering: #center;
		addMorph: column id asMorph;
		yourself
]

{ #category : #accessing }
SpecListFastTableDataSource >> listModel [
	^ model model
]

{ #category : #accessing }
SpecListFastTableDataSource >> menuColumn: column row: rowIndex [
	| menu menuPresenter |
	menu := UIManager default newMenuIn: self table for: self model.
	menuPresenter := self model menu: menu shifted: false.
	^ menuPresenter
]

{ #category : #accessing }
SpecListFastTableDataSource >> model [
	^ model
]

{ #category : #accessing }
SpecListFastTableDataSource >> model: anObject [
	model := anObject
]

{ #category : #accessing }
SpecListFastTableDataSource >> newDataSourceMatching: aFTFilter [
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
SpecListFastTableDataSource >> numberOfRows [
	^ model
		ifNil: [ 0 ]
		ifNotNil: [ self listModel size ]
]

{ #category : #accessing }
SpecListFastTableDataSource >> searchText: aString [
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
SpecListFastTableDataSource >> transferFor: passenger from: aMorph [
    ^(self model transferFor: passenger from: self table) buildWithSpec
]
