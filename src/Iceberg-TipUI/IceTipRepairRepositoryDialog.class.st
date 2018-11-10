Class {
	#name : #IceTipRepairRepositoryDialog,
	#superclass : #IceTipDialog,
	#instVars : [
		'optionsList',
		'detailPanel',
		'model',
		'selectedOption',
		'reasonPanel'
	],
	#category : #'Iceberg-TipUI-View'
}

{ #category : #specs }
IceTipRepairRepositoryDialog class >> defaultSpec [
	<spec: #default>
	
	^ SpecLayout composed
		newColumn: [ :column |
			column 
				newRow: #reasonPanel height: 70 * World displayScaleFactor;
				newRow: #optionsList;
				newRow: #detailPanel ]
]

{ #category : #'instance creation' }
IceTipRepairRepositoryDialog class >> onRepository: aRepository [
	^ self on: (IceTipRepositoryModel on: aRepository) beCached
]

{ #category : #specs }
IceTipRepairRepositoryDialog class >> title [
	^ 'Repair repository'
]

{ #category : #actions }
IceTipRepairRepositoryDialog >> accept [
	self window delete.
	selectedOption executeWith: self model
]

{ #category : #'accessing ui' }
IceTipRepairRepositoryDialog >> detailPanel [
	^ detailPanel
]

{ #category : #'accessing ui' }
IceTipRepairRepositoryDialog >> detailPanel: anObject [
	detailPanel := anObject
]

{ #category : #initialization }
IceTipRepairRepositoryDialog >> initialExtent [
	^ (700@500) scaledByDisplayScaleFactor 
]

{ #category : #initialization }
IceTipRepairRepositoryDialog >> initializeOptionsList [
	optionsList widget 
		addColumn: (IceTipTableColumn newAction: #title icon: #icon);
		dataSource: self newOptionsListDataSource;
		bindKeyCombination: Character cr toAction: [ self accept ];
		onAnnouncement: FTSelectionChanged send: #selectionChanged: to: self;
		onAnnouncement: FTStrongSelectionChanged send: #accept to: self.
]

{ #category : #private }
IceTipRepairRepositoryDialog >> initializeReasonPanel [
	reasonPanel widget
		addMorphBack:
			(IceTipReadOnlyTextMorph new
				hResizing: #spaceFill;
				vResizing: #spaceFill;
				contentsWrapped: self model longStatus;
				yourself)
]

{ #category : #initialization }
IceTipRepairRepositoryDialog >> initializeWidgets [
	reasonPanel := self newIcePanel.
	optionsList := self newIceTable.
	detailPanel := self newIcePanel.
	
	self initializeReasonPanel.
	self initializeOptionsList.
	
	self focusOrder 
		add: self optionsList
]

{ #category : #accessing }
IceTipRepairRepositoryDialog >> model [
	^ model
]

{ #category : #'private factory' }
IceTipRepairRepositoryDialog >> newOptionsListDataSource [
	^ IceTipDataSource new 
		tool: self;
		elements: self model availableRepairOptions;
		yourself
]

{ #category : #'accessing ui' }
IceTipRepairRepositoryDialog >> optionsList [
	^ optionsList
]

{ #category : #'accessing ui' }
IceTipRepairRepositoryDialog >> optionsList: anObject [
	optionsList := anObject
]

{ #category : #accessing }
IceTipRepairRepositoryDialog >> reasonPanel [
	^ reasonPanel
]

{ #category : #accessing }
IceTipRepairRepositoryDialog >> reasonPanel: anObject [
	reasonPanel := anObject
]

{ #category : #private }
IceTipRepairRepositoryDialog >> refreshDetailPanel [
	detailPanel widget removeAllMorphs.
	selectedOption ifNotNil: [ 
		detailPanel widget 
			addMorphBack: (IceTipReadOnlyTextMorph new 
				hResizing: #spaceFill;
				vResizing: #spaceFill;
				contentsWrapped: selectedOption help;
				yourself) ]
]

{ #category : #private }
IceTipRepairRepositoryDialog >> selectionChanged: ann [
	selectedOption := ann newSelectedRowIndexes 
		ifNotEmpty: [ :indexes |(self optionsList widget dataSource elementAt: indexes first) ]
		ifEmpty: [ nil ].
	self refreshDetailPanel
]

{ #category : #initialization }
IceTipRepairRepositoryDialog >> setModelBeforeInitialization: anObject [ 
	model := anObject
]
