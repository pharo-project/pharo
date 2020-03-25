"
I am the adapter used to realise a SpTreeTablePresenter.
I will create a tree table (using FTTableMorph), using the columns TableColumn* as definitions.

Is important notice here that most of the row/cell creation happens on SpMorphicTreeTableDataSource. Check there for some implementation details.
"
Class {
	#name : #SpMorphicTreeTableAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#category : #'Spec2-Adapters-Morphic-Table'
}

{ #category : #accessing }
SpMorphicTreeTableAdapter class >> intercellSpacing [

	^ 2@0
]

{ #category : #initialization }
SpMorphicTreeTableAdapter >> addFocusRotationKeyBindings [
	
	^ super addFocusRotationKeyBindings
]

{ #category : #factory }
SpMorphicTreeTableAdapter >> addModelTo: tableMorph [

	self presenter columns do: [ :each | 
		tableMorph addColumn: (self newColumnFrom: each) ].
	self ensureAtLeastOneColumnIn: tableMorph.

	self presenter selection isMultipleSelection ifTrue: [  
		tableMorph beMultipleSelection ].

	self isShowingColumnHeaders 
		ifTrue: [ tableMorph showColumnHeaders ] 
		ifFalse: [ tableMorph hideColumnHeaders ].

	self isResizable 
		ifTrue: [ tableMorph beResizable ] 
		ifFalse: [ tableMorph beNotResizable ].
		
	self isSearchEnabled
		ifTrue: [ tableMorph enableSearch ]
		ifFalse: [ tableMorph disableFunction ].

	tableMorph setBalloonText: self model help.
	
	tableMorph dataSource: self newDataSource.
	
	self presenter selection isEmpty 
		ifFalse: [ self updateSelectionOf: tableMorph ].
	
	self presenter whenSelectionChangedDo: [ 
		self updateSelectionOf: tableMorph ].
	
	self presenter whenRootsChangedDo: [ 
		tableMorph selectIndexes: #().
		tableMorph dataSource: self newDataSource ].
	
	self presenter whenShowColumnHeadersChangedDo: [ 	
		self isShowingColumnHeaders 
			ifTrue: [ tableMorph showColumnHeaders ] 
			ifFalse: [ tableMorph hideColumnHeaders ] ].
		
	self presenter whenColumnsChangedDo: [
		tableMorph columns: #(). 
		self presenter columns do: [ :each | tableMorph addColumn: (self newColumnFrom: each) ].
		tableMorph dataSource: self newDataSource ].
	
	tableMorph
		onAnnouncement: FTSelectionChanged
			send: #selectionChanged:
			to: self;
		onAnnouncement: FTStrongSelectionChanged
			send: #strongSelectionChanged:
			to: self
]

{ #category : #factory }
SpMorphicTreeTableAdapter >> basicSelectionChanged: ann [
	| selection |
	
	selection := self presenter selection.
	ann newSelectedIndexes ifEmpty: [ ^ selection unselectAll ].
	(ann newSelectedIndexes difference: ann oldSelectedIndexes) ifEmpty: [ ^ self ].
	
	selection selectPaths: (ann newSelectedIndexes
		collect: [ :index | self widget dataSource pathFromIndex: index ])
]

{ #category : #factory }
SpMorphicTreeTableAdapter >> buildWidget [
	| tableMorph |

	tableMorph := FTTableMorph new
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		intercellSpacing: self class intercellSpacing;
		yourself.

	self addModelTo: tableMorph.

	^ tableMorph
]

{ #category : #testing }
SpMorphicTreeTableAdapter >> columnCount [
	
	^ self widget columns size
]

{ #category : #factory }
SpMorphicTreeTableAdapter >> defaultColumn [

	^ SpStringTableColumn new evaluated: #yourself
]

{ #category : #factory }
SpMorphicTreeTableAdapter >> ensureAtLeastOneColumnIn: tableMorph [
	self model columns ifNotEmpty: [ ^ self ].
	tableMorph addColumn: (self newColumnFrom: self defaultColumn)
]

{ #category : #testing }
SpMorphicTreeTableAdapter >> isNonEditableRow: aRow column: aColumn [

	^ (self widget dataSource
		cellColumn: (self widget columns at: aColumn)
		row: aRow) submorphs 
		allSatisfy: [ :e | e asReadOnlyMorph == e ]
]

{ #category : #testing }
SpMorphicTreeTableAdapter >> isResizable [

	^ self presenter isResizable
]

{ #category : #testing }
SpMorphicTreeTableAdapter >> isSearchEnabled [

	^ self presenter isSearchEnabled
]

{ #category : #testing }
SpMorphicTreeTableAdapter >> isShowingColumnHeaders [

	^ self presenter isShowingColumnHeaders
]

{ #category : #'private factory' }
SpMorphicTreeTableAdapter >> newColumnFrom: aTableColumn [

	^ (SpMorphicTableColumn on: aTableColumn)
		width: aTableColumn width;
		yourself
]

{ #category : #'private factory' }
SpMorphicTreeTableAdapter >> newDataSource [ 

	^ SpMorphicTreeTableDataSource new
		model: self model; 
		rootItem: (self rootForItems: self model roots);
		childrenBlock: [ :data :item | self model childrenFor: data ];
		yourself
]

{ #category : #emulating }
SpMorphicTreeTableAdapter >> rightClicked [
	| evt |

	evt := MouseButtonEvent new 
		setType: nil 
		position: widget center 
		which: MouseButtonEvent yellowButton 
		buttons: MouseButtonEvent yellowButton 
		hand: nil 
		stamp: nil.
	
	self widget click: evt
]

{ #category : #private }
SpMorphicTreeTableAdapter >> rootForItems: aCollection [

	^ FTRootItem new
		data: aCollection;
		yourself
]

{ #category : #selecting }
SpMorphicTreeTableAdapter >> selectInto: aFTTableMorph paths: aCollectionOfPaths [ 
	
	| indexes items |
	
	items := aCollectionOfPaths collect: [ :aPath | | item |
		item := aFTTableMorph dataSource itemAtPath: aPath expanding: true.
		item 
			ifNil: [ aFTTableMorph selectIndex: 0. ^ self ].
		item ].

	aFTTableMorph refresh.
	indexes := items collect: [:item | aFTTableMorph dataSource indexOfItem: item].

	aFTTableMorph selectIndexes: indexes.
]

{ #category : #accessing }
SpMorphicTreeTableAdapter >> selectPath: anArray [
	| index |
	
	((anArray isEmpty or: [ anArray first = 0 ]) 
		and: [ widget selectedIndexes isEmpty ]) 
		ifTrue: [ ^ self ].
	index := anArray 
		ifNotEmpty: [ widget dataSource indexFromPath: anArray ]
		ifEmpty: [ 0 ].  
	(widget selectedIndexes includes: index) ifTrue: [ ^ self ].
	widget selectIndex: index
]

{ #category : #accessing }
SpMorphicTreeTableAdapter >> selectedItem [
	
	| selectedIndex |
	selectedIndex := self widget selectedIndex.
	
	selectedIndex = 0 ifTrue: [ ^ nil ].
	
	^ (self widget dataSource elementAt: selectedIndex) data
]

{ #category : #accessing }
SpMorphicTreeTableAdapter >> selectedPath [
	
	^ self selectedPaths first 
]

{ #category : #accessing }
SpMorphicTreeTableAdapter >> selectedPaths [
	
	^ self widget selectedIndexes collect: [ :e | self widget dataSource pathFromIndex: e  ] 
]

{ #category : #factory }
SpMorphicTreeTableAdapter >> selectionChanged: ann [

	self suspendSelectionEventsDuring: [ 
		self basicSelectionChanged: ann ]
]

{ #category : #factory }
SpMorphicTreeTableAdapter >> strongSelectionChanged: aFTStrongSelectionChanged [
	self presenter activatesOnDoubleClick
		ifFalse: [ ^ self ].
	self presenter doubleClickAtPath: self selectedPath
]

{ #category : #factory }
SpMorphicTreeTableAdapter >> suspendSelectionEventsDuring: aBlock [
	| registry subscriptions |

	registry := self widget announcer subscriptions.
	subscriptions := Set new.
	registry 
		subscriptionsOf: self 
		do: [ :each | 
			({ FTSelectionChanged. FTStrongSelectionChanged } includes: each announcementClass) 
				ifTrue: [ subscriptions add: each ] ]. 
			
	subscriptions do: [ :each | registry remove: each ].
	aBlock ensure: [ 
		 subscriptions do: [ :each | registry add: each ] ]
]

{ #category : #update }
SpMorphicTreeTableAdapter >> updateMenu [
	"there is no need to updateMenu here because it will be resolved at the dataSource"
]

{ #category : #selection }
SpMorphicTreeTableAdapter >> updateSelectionOf: tableMorph [
	
	self 
		selectInto: tableMorph 
		paths: self model selection selectedPaths
]
