Class {
	#name : #IceTipTreeSelectingDataSource,
	#superclass : #IceTipTreeDataSource,
	#instVars : [
		'selectedItems'
	],
	#category : #'Iceberg-TipUI-View-DataSource'
}

{ #category : #private }
IceTipTreeSelectingDataSource >> addBindingsToTable [	
	self table
		bindKeyCombination: Character space 
		toAction: [ self toggleSelectionOf: self selectedItem ]
]

{ #category : #private }
IceTipTreeSelectingDataSource >> deselectAllChildrenOf: item [
	(self withAllChildrenOf: item) 
		do: [ :each | selectedItems remove: each data ifAbsent: [] ].
	self tableRefresh
]

{ #category : #initialization }
IceTipTreeSelectingDataSource >> initialize [
	selectedItems := Set new.
	super initialize.

]

{ #category : #private }
IceTipTreeSelectingDataSource >> isSelected: anObject [
	^ selectedItems includes: anObject
]

{ #category : #'private factory' }
IceTipTreeSelectingDataSource >> newCellFor: item [
	| cell |
	
	cell := FTIndentedCellMorph new.
	cell indentBy: item depth * 16.
	cell addMorphBack: (self buttonFor: item).
	cell addMorphBack: (self newSelectionMorphFor: item).
	(self iconFor: item data) 
		ifNotNil: [ :icon | cell addMorphBack: icon asMorph ].
	cell addMorphBack: (self toString: item data) asMorph.
	
	^ cell
]

{ #category : #'private factory' }
IceTipTreeSelectingDataSource >> newSelectionMorphFor: item [
	^ CheckboxButtonMorph new
		selected: (self isSelected: item data);
		target: [ self toggleSelectionOf: item ];
		actionSelector: #value;
		yourself	
]

{ #category : #accessing }
IceTipTreeSelectingDataSource >> rootForItems: aCollection [
 	selectedItems := Set new.	
	self rootItem: (FTRootItem new
		data: aCollection;
		yourself)
]

{ #category : #accessing }
IceTipTreeSelectingDataSource >> selectAll [
	selectedItems := Set new.
	self rootItem children
		do: [ :each | self selectAllChildrenOf: each ].
	self tableRefresh
]

{ #category : #private }
IceTipTreeSelectingDataSource >> selectAllChildrenOf: item [
	(self withAllChildrenOf: item) 
		do: [ :each | selectedItems add: each data ]
]

{ #category : #private }
IceTipTreeSelectingDataSource >> selectedItem [
	^ self table selectedRowIndexes 
		ifNotEmpty: [ :indexes | self elementAt: indexes first ]
		ifEmpty: [ nil ]
]

{ #category : #accessing }
IceTipTreeSelectingDataSource >> selectedItems [
	^ selectedItems
]

{ #category : #accessing }
IceTipTreeSelectingDataSource >> selectedItems: aCollection [
	selectedItems := aCollection asSet.
	self tableRefresh
]

{ #category : #accessing }
IceTipTreeSelectingDataSource >> table: aTable [
	super table: aTable.
	self addBindingsToTable
]

{ #category : #private }
IceTipTreeSelectingDataSource >> tableRefresh [
	self table ifNil: [ ^ self ].
	^ super tableRefresh 
]

{ #category : #private }
IceTipTreeSelectingDataSource >> toggleSelectionOf: item [
	(self isSelected: item data) 
		ifTrue: [ self deselectAllChildrenOf: item ]
		ifFalse: [ self selectAllChildrenOf: item ].
	"self glamourPresentation cherryPick: selection."
	self tableRefresh

]

{ #category : #private }
IceTipTreeSelectingDataSource >> withAllChildrenOf: item [
	^ item children
		inject: { item }
		into: [ :all :each | all, (self withAllChildrenOf: each) ]
]
