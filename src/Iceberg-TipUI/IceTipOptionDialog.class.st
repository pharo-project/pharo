"
I'm a dialog that has different options. 
I'm a abstract class, I need to be used  through my children.

I'm kind of a ""tabbed"" dialog, but my options are ""mac style"": lateral and not top.

This is useful to keep a variable number of options.
"
Class {
	#name : #IceTipOptionDialog,
	#superclass : #IceTipDialog,
	#instVars : [
		'typeList',
		'type',
		'contentPanel'
	],
	#category : #'Iceberg-TipUI-View'
}

{ #category : #specs }
IceTipOptionDialog class >> defaultSpec [
	<spec>
	
	^ SpecLayout composed
		newRow: [ :row | 
			row
				newColumn: #typeList width: 180 * World displayScaleFactor;
				newColumn: #contentPanel ]
]

{ #category : #actions }
IceTipOptionDialog >> accept [
	^ [ self doAccept.
	true ]
		on: IceError , IceWarning
		do: [ :e | 
			e acceptError: IceTipInteractiveErrorVisitor new.
			false ]
]

{ #category : #accessing }
IceTipOptionDialog >> allTypes [
	^ self subclassResponsibility
]

{ #category : #private }
IceTipOptionDialog >> basicSelectionChanged: aType [
	type := aType.
	aType 
		ifNotNil: [ 	
			self title: type title.
			self replacePanelWith: type ]
		ifNil: [ 
			self removeAllPanels.
			self title: self class title ]
]

{ #category : #'accessing ui' }
IceTipOptionDialog >> contentPanel [
	^ contentPanel
]

{ #category : #'accessing ui' }
IceTipOptionDialog >> contentPanel: anObject [
	contentPanel := anObject
]

{ #category : #accessing }
IceTipOptionDialog >> contextSelection [
	^ type
]

{ #category : #actions }
IceTipOptionDialog >> doAccept [
	self subclassResponsibility
]

{ #category : #private }
IceTipOptionDialog >> emptyPanel [
	^ PanelMorph new
		changeProportionalLayout;
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		yourself	
]

{ #category : #initialization }
IceTipOptionDialog >> giveFocusToNextFrom: aModel [

	super giveFocusToNextFrom: aModel
]

{ #category : #accessing }
IceTipOptionDialog >> initialExtent [
	^ (600@300) scaledByDisplayScaleFactor
]

{ #category : #initialization }
IceTipOptionDialog >> initializeTypeList [
	typeList widget 
		addColumn: (IceTipTableColumn newAction: #title icon: #icon);
		dataSource: self newTypeListDataSource;
		onAnnouncement: FTSelectionChanged send: #selectionChanged: to: self.
]

{ #category : #initialization }
IceTipOptionDialog >> initializeWidgets [
	contentPanel := self emptyPanel asSpecAdapter.
	typeList := self newIceTable.
	self initializeTypeList.
			
	self whenBuiltDo: [ :ann |
		self addShortcutsTo: ann widget.
		self selectFirst ].

	self eventKeyStrokesForNextFocus do: [:each |
		typeList widget bindKeyCombination: each toAction: [ self giveFocusToNextFrom: typeList ] ].
	self eventKeyStrokesForPreviousFocus do: [:each |
		typeList widget bindKeyCombination: each toAction: [ self giveFocusToPreviousFrom: typeList ] ].
		
	self refreshFocusOrder.
]

{ #category : #'private factory' }
IceTipOptionDialog >> newTypeListDataSource [
	^ IceTipDataSource new 
		tool: self;
		elements: self allTypes;
		yourself.
		
	"^ IceTipTreeOutlineDataSource new 
		tool: self;
		rootForItems: self allTypes;
		childrenBlock: [ :each | #() ];
		format: [ :each | each title ];
		icon: [ :each | each icon ];
		yourself"
]

{ #category : #focus }
IceTipOptionDialog >> refreshFocusOrder [
	
	self focusOrder
		removeAll;
		add: self typeList;
		add: self selectedType;
		add: acceptButton;
		add: cancelButton
]

{ #category : #private }
IceTipOptionDialog >> removeAllPanels [
	| panelMorph |
	
	panelMorph := self contentPanel morph.
	panelMorph removeAllMorphs
]

{ #category : #private }
IceTipOptionDialog >> replacePanelWith: aType [
	| panelMorph specPanel |
	
	panelMorph := self contentPanel morph.
	panelMorph removeAllMorphs.
	
	aType ifNil: [ ^ self ].
	specPanel := aType buildWithSpec.
	panelMorph 
		addMorph: specPanel
		fullFrame: LayoutFrame identity.

	self eventKeyStrokesForNextFocus do: [:each |
		aType bindKeyCombination: each toAction: [ self giveFocusToNextFrom: specPanel ] ].
	self eventKeyStrokesForPreviousFocus do: [:each |
		aType bindKeyCombination: each toAction: [ self giveFocusToPreviousFrom: specPanel ] ].

	self refreshFocusOrder
]

{ #category : #private }
IceTipOptionDialog >> selectFirst [
	typeList widget selectRowIndex: 1.
	self basicSelectionChanged: self allTypes first 	
]

{ #category : #accessing }
IceTipOptionDialog >> selectedType [
	^ type
]

{ #category : #private }
IceTipOptionDialog >> selectionChanged: ann [
	self basicSelectionChanged: (ann newSelectedRowIndexes
		ifNotEmpty: [ :indexes |	self typeList widget dataSource realElementAt: indexes first ]
		ifEmpty: [ nil ])
]

{ #category : #'accessing ui' }
IceTipOptionDialog >> typeList [
	^ typeList
]

{ #category : #'accessing ui' }
IceTipOptionDialog >> typeList: anObject [
	typeList := anObject
]
