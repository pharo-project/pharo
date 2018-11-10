"
I'm a diff panel that shows packages/classes/methods changed in a column and a diff in the other.

I receive a model that needs to understand #elements.
also #elements answer should be a collection of elements that also understand elements.

(IceTipDiffPanel onDiff: (IceRepository registry first workingCopyDiff)) openWithSpec
"
Class {
	#name : #IceTipDiffPanel,
	#superclass : #IceTipComposablePresenter,
	#instVars : [
		'model',
		'changeList',
		'diffPanel'
	],
	#category : #'Iceberg-TipUI-View'
}

{ #category : #specs }
IceTipDiffPanel class >> defaultSpec [
	<spec: #default>
	
	^ SpecLayout composed
		newRow: [ :row |
			row 
				newColumn: #changeList right: 0.7;
				addVSplitter; 
				newColumn: #diffPanel left: 0.3 ]
]

{ #category : #'instance creation' }
IceTipDiffPanel class >> onDiff: aDiff [
	^ self on: (IceTipCommitDiffModel on: aDiff) beCached
]

{ #category : #specs }
IceTipDiffPanel class >> title [
	^ 'Diff'
]

{ #category : #accessing }
IceTipDiffPanel >> basicRefresh [
	self resetDiffContents.
	changeList widget 
		in: [ :this |
			this selectRowIndexes: #(). 
			self resetDataSource: this dataSource ];
		refresh.

]

{ #category : #'accessing ui' }
IceTipDiffPanel >> changeList [
	^ changeList
]

{ #category : #'accessing ui' }
IceTipDiffPanel >> changeList: anObject [
	changeList := anObject
]

{ #category : #private }
IceTipDiffPanel >> changeSelected [
	| index |
	self flag: #todo. "This should -maybe- be modeled better on a SpecTablePresenter ;)"
	index := changeList widget selectedRowIndex.
	index = 0 ifTrue: [ ^ nil ].
	^ changeList widget dataSource realElementAt: index
]

{ #category : #accessing }
IceTipDiffPanel >> contextClass [
	^ IceTipDiffContext
]

{ #category : #accessing }
IceTipDiffPanel >> contextSelection [ 
	^ self changeSelected
]

{ #category : #private }
IceTipDiffPanel >> diffContentsLeft: leftString right: rightString [

	diffPanel
		"Clean the diff first, to avoid comparing old right against new left"
		leftText: '';
		rightText: '';
		"Now set all of them"
		leftText: leftString;
		rightText: rightString
]

{ #category : #'accessing ui' }
IceTipDiffPanel >> diffPanel [
	^ diffPanel
]

{ #category : #'accessing ui' }
IceTipDiffPanel >> diffPanel: anObject [
	diffPanel := anObject
]

{ #category : #initialization }
IceTipDiffPanel >> initializeChangeList [
	| dataSource |
	dataSource := self newChangeTreeDataSource.
	changeList widget dataSource: dataSource.
	dataSource expandAll
]

{ #category : #initialization }
IceTipDiffPanel >> initializeDiffPanel [
	diffPanel showOptions: false
]

{ #category : #initialization }
IceTipDiffPanel >> initializeWidgets [
	changeList := self newIceTable.
	diffPanel := self instantiate: DiffPresenter.
	
	self initializeChangeList.
	self initializeDiffPanel.
	
	changeList widget
		onAnnouncement: FTSelectionChanged 
		send: #selectionChanged:
		to: self.
		
	self whenBuiltDo: [ :ann | self addShortcutsTo: ann widget ].
	
	self focusOrder 
		add: changeList;
		add: diffPanel
]

{ #category : #accessing }
IceTipDiffPanel >> leftLabel: aString [ 
	"This allows us to be compatible with old pharo versions"
	(diffPanel respondsTo: #leftLabel:) ifFalse: [ ^ self ].
	diffPanel leftLabel: aString.
]

{ #category : #accessing }
IceTipDiffPanel >> model [
	^ model
]

{ #category : #accessing }
IceTipDiffPanel >> model: anObject [
	model := anObject.
	self refresh
]

{ #category : #'private factory' }
IceTipDiffPanel >> newChangeTreeDataSource [
	^ IceTipTreeDataSource new 
		tool: self;
		rootForItems: self model treeRoots;
		childrenBlock: [ :data | data children ];
		icon: [ :each | each value icon ];
		format: #description;
		yourself
]

{ #category : #'private factory' }
IceTipDiffPanel >> newContext [
	^ super newContext
	repositoryModel: self model repositoryModel;
	yourself
]

{ #category : #'private factory' }
IceTipDiffPanel >> newContextWithSelection: anObject [
	^ self newContext
	item: anObject;
	yourself
]

{ #category : #accessing }
IceTipDiffPanel >> refresh [
	self model reset.
	self basicRefresh
]

{ #category : #private }
IceTipDiffPanel >> resetDataSource: aDataSource [
	^ aDataSource 
		rootForItems: self model treeRoots;
		expandAll
]

{ #category : #private }
IceTipDiffPanel >> resetDiffContents [
	self diffContentsLeft: '' right: ''
]

{ #category : #accessing }
IceTipDiffPanel >> rightLabel: aString [ 
	"This allows us to be compatible with old pharo versions"
	(diffPanel respondsTo: #leftLabel:) ifFalse: [ ^ self ].
	diffPanel rightLabel: aString
]

{ #category : #'event handling' }
IceTipDiffPanel >> selectionChanged: ann [
	| element |
	(ann newSelectedRowIndexes reject: [ :each | each = 0 ])
		ifNotEmpty: [ :indexes | 
			element := changeList widget dataSource realElementAt: indexes first.
			self 
				diffContentsLeft:  element value rightContents
				right: element value leftContents ] 
		ifEmpty: [ 
			self resetDiffContents ].
	
	
]

{ #category : #initialization }
IceTipDiffPanel >> setModelBeforeInitialization: anObject [
	model := anObject
]
