Class {
	#name : #SpComponentListFastTableDataSource,
	#superclass : #FTDataSource,
	#instVars : [
		'model'
	],
	#category : #'Spec2-Adapters-Morphic-ComponentList'
}

{ #category : #accessing }
SpComponentListFastTableDataSource >> cellColumn: column row: rowIndex [
	| displayValue cell item |
	item := self model presenters at: rowIndex.
	cell := FTCellMorph new
		cellInset: 5;
		yourself.
	cell addMorphBack: item buildWithSpec.
	^ cell
]

{ #category : #accessing }
SpComponentListFastTableDataSource >> elementAt: rowIndex [

	^ self listModel at: rowIndex
]

{ #category : #accessing }
SpComponentListFastTableDataSource >> elements [

	^ self model items
]

{ #category : #accessing }
SpComponentListFastTableDataSource >> headerColumn: column [
	column id ifNil: [ ^ nil ]. 
	^ FTCellMorph new 
		listCentering: #center;
		addMorph: column id asMorph asReadOnlyMorph;
		yourself
]

{ #category : #accessing }
SpComponentListFastTableDataSource >> listModel [
	^ model model
]

{ #category : #accessing }
SpComponentListFastTableDataSource >> menuColumn: column row: rowIndex [
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
SpComponentListFastTableDataSource >> model [
	^ model
]

{ #category : #accessing }
SpComponentListFastTableDataSource >> model: anObject [
	model := anObject
]

{ #category : #accessing }
SpComponentListFastTableDataSource >> newDataSourceMatching: aFTFilter [
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
SpComponentListFastTableDataSource >> numberOfRows [
	^ model
		ifNil: [ 0 ]
		ifNotNil: [ model presenters size ]
]

{ #category : #accessing }
SpComponentListFastTableDataSource >> searchText: aString [
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
SpComponentListFastTableDataSource >> transferFor: passenger from: aMorph [
    ^(self model transferFor: passenger from: self table) buildWithSpec
]
