"
I'm a generic datasource for tables on Tip tools. 
I use IceTipTableColumn to determine value to show. 
I use a menuSelectionContext to be able to show contextual menus.
"
Class {
	#name : #IceTipDataSource,
	#superclass : #FTSimpleDataSource,
	#instVars : [
		'tool'
	],
	#category : #'Iceberg-TipUI-View-DataSource'
}

{ #category : #accessing }
IceTipDataSource >> actionColumn: column [
	"If columns has actions associated, execute them"
	self flag: #todo.
	self tableRefresh
]

{ #category : #accessing }
IceTipDataSource >> cellColumn: column row: rowIndex [
	"Answer a morph with the cell view. I will probably return a FTCellMorph."
	| item cell |

	item := self elementAt: rowIndex.
	cell := FTCellMorph new
		cellInset: 5;
		yourself.		
	(column readIcon: item) 
		ifNotNil: [ :icon | cell addMorphBack: icon asMorph ]. 
	cell addMorphBack: (IceTipLabelMorph new contentsAsIs: (column read: item)).

	^ cell
]

{ #category : #accessing }
IceTipDataSource >> elementAt: aNumber ifAbsent: aBlock [ 
	^ self elements
		at: aNumber 
		ifAbsent: aBlock
]

{ #category : #testing }
IceTipDataSource >> hasElements [
	^ self elements notEmpty
]

{ #category : #accessing }
IceTipDataSource >> headerColumn: column [
	column id ifNil: [ ^ nil ].
	^ column newHeader 
		target: self;
		actionSelector: #actionColumn:;
		arguments: {column};	
		yourself
]

{ #category : #accessing }
IceTipDataSource >> menuColumn: column row: rowIndex [
	| menuSelection context |
	
	rowIndex = 0 ifTrue: [ ^ nil ].
	menuSelection := self elementAt: rowIndex.
	context := self menuSelectionContextWithSelection: menuSelection.		
	^ CmdContextMenuActivation 	
		buildContextMenuFor: self table 
		inContext: context
]

{ #category : #accessing }
IceTipDataSource >> menuSelectionContext [
	^ self tool newContext
]

{ #category : #accessing }
IceTipDataSource >> menuSelectionContextWithSelection: anObject [
	^ self tool newContextWithSelection: anObject
]

{ #category : #factory }
IceTipDataSource >> newDataSourceMatching: aFilter [
	^ aFilter executeOn: self
]

{ #category : #accessing }
IceTipDataSource >> tool [
	^ tool
]

{ #category : #accessing }
IceTipDataSource >> tool: aTool [
	tool := aTool
]
