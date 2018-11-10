"
Description
--------------------

I am a datasource which elements are dislpayed with a checkbox to be able to select some of them. 

Public API and Key Messages
--------------------

- #selectedItems 					Accessor to get the selected items
- #onSelectionChangedBlock 		Optional block executed when an item is selected/deselected. It takes as first parameter the item and as second parameter a boolean (true on selection, false on deselection)

Examples
--------------------

	IceTipSelectingDataSource new
		tool: self;
		elements: self allPackagesNotRegistered;
		yourself	
 
Internal Representation and Key Implementation Points.
--------------------

    Instance Variables
	onSelectionChangedBlock:		<aBlock>		Optional block executed when an item is selected/deselected. It takes as first parameter the item and as second parameter a boolean (true on selection, false on deselection)
	selectedItems:					<aSet>			Collection containing all the selected items

"
Class {
	#name : #IceTipSelectingDataSource,
	#superclass : #IceTipDataSource,
	#instVars : [
		'selectedItems',
		'onSelectionChangedBlock'
	],
	#category : #'Iceberg-TipUI-View-DataSource'
}

{ #category : #private }
IceTipSelectingDataSource >> add: item [
	self selectedItems add: item.
	self onSelectionChangedBlock ifNotNil: [ :block | block cull: item cull: true ]
	
]

{ #category : #private }
IceTipSelectingDataSource >> addBindingsToTable [	
	self table
		bindKeyCombination: Character space 
		toAction: [ self toggleSelectionOf: self selectedItem ]
]

{ #category : #accessing }
IceTipSelectingDataSource >> cellColumn: column row: rowIndex [
	"Answer a morph with the cell view. I will probably return a FTCellMorph."
	| item cell |

	item := self elementAt: rowIndex.
	cell := FTCellMorph new
		cellInset: 5;
		yourself.		
	cell addMorphBack: (self newSelectionMorphFor: item).
	(column readIcon: item) 
		ifNotNil: [ :icon | cell addMorphBack: icon asMorph ]. 
	cell addMorphBack: (column read: item) asMorph asReadOnlyMorph.

	^ cell
]

{ #category : #initialization }
IceTipSelectingDataSource >> initialize [
	selectedItems := Set new.
	super initialize
]

{ #category : #testing }
IceTipSelectingDataSource >> isSelected: anObject [
	^ selectedItems includes: anObject
]

{ #category : #'private factory' }
IceTipSelectingDataSource >> newSelectionMorphFor: item [
	^ CheckboxButtonMorph new
		selected: (self isSelected: item);
		target: [ self toggleSelectionOf: item ];
		actionSelector: #value;
		yourself	
]

{ #category : #accessing }
IceTipSelectingDataSource >> onSelectionChangedBlock [
	^ onSelectionChangedBlock
]

{ #category : #accessing }
IceTipSelectingDataSource >> onSelectionChangedBlock: anObject [
	onSelectionChangedBlock := anObject
]

{ #category : #private }
IceTipSelectingDataSource >> remove: item [
	self selectedItems remove: item.
	self onSelectionChangedBlock ifNotNil: [ :block | block cull: item cull: false ]
]

{ #category : #selection }
IceTipSelectingDataSource >> selectAll [
	self elements
		do: [ :each | selectedItems add: each ].
	self tableRefresh
]

{ #category : #private }
IceTipSelectingDataSource >> selectedItem [
	^ self table selectedRowIndexes 
		ifNotEmpty: [ :indexes | self elementAt: indexes first ]
		ifEmpty: [ nil ]
]

{ #category : #accessing }
IceTipSelectingDataSource >> selectedItems [
	^ selectedItems
]

{ #category : #accessing }
IceTipSelectingDataSource >> table: aTable [
	super table: aTable.
	self addBindingsToTable
]

{ #category : #private }
IceTipSelectingDataSource >> toggleSelectionOf: item [
	(self isSelected: item)
		ifTrue: [ self remove: item ]
		ifFalse: [ self add: item ].
	self tableRefresh
]
