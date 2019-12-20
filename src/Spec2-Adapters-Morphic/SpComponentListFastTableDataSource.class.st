Class {
	#name : #SpComponentListFastTableDataSource,
	#superclass : #FTDataSource,
	#instVars : [
		'model'
	],
	#category : #'Spec2-Adapters-Morphic-List'
}

{ #category : #accessing }
SpComponentListFastTableDataSource >> cellColumn: column row: rowIndex [
	| cell item |
	
	item := self elementAt: rowIndex.
	cell := FTCellMorph new
		cellInset: 5;
		yourself.
	cell addMorphBack: (self widgetFor: item).
	
	^ cell
]

{ #category : #accessing }
SpComponentListFastTableDataSource >> elementAt: rowIndex [

	^ self presenters at: rowIndex
]

{ #category : #accessing }
SpComponentListFastTableDataSource >> elements [

	^ self presenters
]

{ #category : #accessing }
SpComponentListFastTableDataSource >> headerColumn: column [

	column id ifNil: [ ^ nil ]. 
	^ FTCellMorph new 
		listCentering: #left;
		addMorph: column id asMorph asReadOnlyMorph;
		yourself
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
		ifNotNil: [ self presenters size ]
]

{ #category : #accessing }
SpComponentListFastTableDataSource >> presenters [

	^ self model presenters
]

{ #category : #accessing }
SpComponentListFastTableDataSource >> rowHeight: rowIndex [

	rowIndex = 0 ifTrue: [ ^ super rowHeight: rowIndex ].
	^ (self widgetFor: (self elementAt: rowIndex)) height
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

{ #category : #private }
SpComponentListFastTableDataSource >> widgetFor: aPresenter [

	aPresenter adapter ifNotNil: [ :adapter | ^ adapter widget ].
	^ aPresenter buildWithSpec
		vResizing: #shrinkWrap;
		yourself
]
