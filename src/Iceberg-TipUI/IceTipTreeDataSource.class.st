"
I'm a generic tree datasource for Tip components.
"
Class {
	#name : #IceTipTreeDataSource,
	#superclass : #FTTreeDataSource,
	#instVars : [
		'tool',
		'formatBlock',
		'iconBlock'
	],
	#category : #'Iceberg-TipUI-View-DataSource'
}

{ #category : #accessing }
IceTipTreeDataSource >> cellColumn: column row: rowIndex [
	^ self newCellFor: (self elementAt: rowIndex)
]

{ #category : #'private factory' }
IceTipTreeDataSource >> cellIndentFor: item [
	^ item depth * 16
]

{ #category : #accessing }
IceTipTreeDataSource >> format [
	^ formatBlock
]

{ #category : #accessing }
IceTipTreeDataSource >> format: anObject [
	formatBlock := anObject
]

{ #category : #accessing }
IceTipTreeDataSource >> icon [
	^ iconBlock
]

{ #category : #accessing }
IceTipTreeDataSource >> icon: aBlock [
	iconBlock := aBlock
]

{ #category : #accessing }
IceTipTreeDataSource >> iconFor: item [
	^ iconBlock 
		ifNotNil: [ iconBlock value: item ]
		ifNil: [ nil ]
]

{ #category : #accessing }
IceTipTreeDataSource >> indexOfElement: anObject [
	1 to: self numberOfRows do: [ :index |
		(self realElementAt: index) = anObject 
			ifTrue: [ ^ index ] ].
	^ 0
]

{ #category : #accessing }
IceTipTreeDataSource >> indexOfElementMatching: aBlock [
	1 to: self numberOfRows do: [ :index |
		(aBlock value: (self realElementAt: index)) 
			ifTrue: [ ^ index ] ].
	^ 0
]

{ #category : #accessing }
IceTipTreeDataSource >> menuColumn: column row: rowIndex [
	| menuSelection context |

	menuSelection := self elementAt: rowIndex.
	context := self menuSelectionContextWithSelection: menuSelection data.
	^ CmdContextMenuActivation 
		buildContextMenuFor: self table
		inContext: context
]

{ #category : #accessing }
IceTipTreeDataSource >> menuSelectionContext [
	^ self tool newContext
]

{ #category : #accessing }
IceTipTreeDataSource >> menuSelectionContextWithSelection: anObject [
	^ self tool newContextWithSelection: anObject
]

{ #category : #'private factory' }
IceTipTreeDataSource >> newCellFor: item [
	| cell |
	
	cell := FTIndentedCellMorph new.
	cell indentBy: (self cellIndentFor: item).
	cell addMorphBack: (self buttonFor: item).
	(self iconFor: item data) 
		ifNotNil: [ :icon | cell addMorphBack: icon asMorph ].
	cell addMorphBack: (self toString: item data) asMorph asReadOnlyMorph.
	
	^ cell 
]

{ #category : #factory }
IceTipTreeDataSource >> newDataSourceMatching: aFilter [
	^ aFilter executeOn: self
]

{ #category : #accessing }
IceTipTreeDataSource >> rootForItems: aCollection [
 	self rootItem: (FTRootItem new
		data: aCollection;
		yourself)
]

{ #category : #accessing }
IceTipTreeDataSource >> rootItem: anItem [
	super rootItem: anItem.
	self addSourceToRootItem
]

{ #category : #accessing }
IceTipTreeDataSource >> searchText: aString [
	"I use the search strategy specific for IceTip, as the elements are FTBasicItem and should be open"

	^ IceTipTreeSearchStrategy searchWith: aString dataSource: self
]

{ #category : #accessing }
IceTipTreeDataSource >> toString: anObject [
	^ self format 
		ifNotNil: [ self format value: anObject ]
		ifNil: [ anObject ]
]

{ #category : #accessing }
IceTipTreeDataSource >> tool [
	^ tool
]

{ #category : #accessing }
IceTipTreeDataSource >> tool: anObject [
	tool := anObject
]
