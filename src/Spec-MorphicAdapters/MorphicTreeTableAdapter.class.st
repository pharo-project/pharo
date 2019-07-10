"
I am the adapter used to realise a TreeTablePresenter.
I will create a tree table (using FTTableMorph), using the columns TableColumn* as definitions.

Is important notice here that most of the row/cell creation happens on MorphicTreeTableDataSource. Check there for some implementation details.
"
Class {
	#name : #MorphicTreeTableAdapter,
	#superclass : #AbstractMorphicAdapter,
	#category : #'Spec-MorphicAdapters-Table'
}

{ #category : #factory }
MorphicTreeTableAdapter >> addModelTo: tableMorph [

	self model columns do: [ :each | 
		tableMorph addColumn: (self newColumnFrom: each) ].
	self ensureAtLeastOneColumnIn: tableMorph.

	self model selection isMultipleSelection ifTrue: [  
		tableMorph beMultipleSelection ].

	self isShowingColumnHeaders 
		ifTrue: [ tableMorph showColumnHeaders ] 
		ifFalse: [ tableMorph hideColumnHeaders ].

	self isResizable 
		ifTrue: [ tableMorph beResizable ] 
		ifFalse: [ tableMorph beNotResizable ].

	tableMorph setBalloonText: self model help.
	
	tableMorph dataSource: self newDataSource.
	
	self model selection isEmpty 
		ifFalse: [ self updateSelectionOf: tableMorph ].
	
	self model whenSelectionChangedDo: [ self updateSelectionOf: tableMorph ].
	self model whenRootsChangedDo: [ 
		tableMorph selectIndexes: #().
		tableMorph dataSource: self newDataSource ].
	
	self model whenShowColumnHeadersChangedDo: [ 	
		self isShowingColumnHeaders 
			ifTrue: [ tableMorph showColumnHeaders ] 
			ifFalse: [ tableMorph hideColumnHeaders ] ].
		
	self model whenColumnsChangedDo: [
		tableMorph columns: #(). 
		self model columns do: [ :each | tableMorph addColumn: (self newColumnFrom: each) ].
		tableMorph dataSource: self newDataSource ].
	
	tableMorph
		onAnnouncement: FTSelectionChanged
			send: #selectionChanged:
			to: self;
		onAnnouncement: FTStrongSelectionChanged
			send: #strongSelectionChanged:
			to: self.
]

{ #category : #factory }
MorphicTreeTableAdapter >> basicSelectionChanged: ann [
	| selection |
	selection := self presenter selection.
	ann newSelectedIndexes ifEmpty: [ ^ selection unselectAll ].
	(ann newSelectedIndexes difference: ann oldSelectedIndexes) ifEmpty: [ ^ self ].
	
	selection
		selectPaths:
			(ann newSelectedIndexes
				collect: [ :i | self widget dataSource pathFromIndex: i ])
]

{ #category : #factory }
MorphicTreeTableAdapter >> buildWidget [
	| tableMorph |

	tableMorph := FTTableMorph new
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		yourself.

	self addModelTo: tableMorph.

	^ tableMorph
]

{ #category : #testing }
MorphicTreeTableAdapter >> columnCount [
	
	^ self widget columns size
]

{ #category : #factory }
MorphicTreeTableAdapter >> defaultColumn [

	^ StringTableColumn new evaluated: #yourself
]

{ #category : #factory }
MorphicTreeTableAdapter >> ensureAtLeastOneColumnIn: tableMorph [
	self model columns ifNotEmpty: [ ^ self ].
	tableMorph addColumn: (self newColumnFrom: self defaultColumn)
]

{ #category : #testing }
MorphicTreeTableAdapter >> isResizable [
	^ self model isResizable
]

{ #category : #testing }
MorphicTreeTableAdapter >> isShowingColumnHeaders [
	^ self model isShowingColumnHeaders
]

{ #category : #'private factory' }
MorphicTreeTableAdapter >> newColumnFrom: aTableColumn [

	^ (MorphicTableColumn on: aTableColumn)
		width: aTableColumn width;
		yourself
]

{ #category : #'private factory' }
MorphicTreeTableAdapter >> newDataSource [ 

	^ MorphicTreeTableDataSource new
		model: self model; 
		rootItem: (self rootForItems: self model roots);
		childrenBlock: [ :data :item | self model childrenFor: data ];
		yourself
]

{ #category : #private }
MorphicTreeTableAdapter >> rootForItems: aCollection [

	^ FTRootItem new
		data: aCollection;
		yourself
]

{ #category : #selecting }
MorphicTreeTableAdapter >> selectInto: aFTTableMorph paths: aCollectionOfPaths [ 
	
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
MorphicTreeTableAdapter >> selectedItem [
	
	| selectedIndex |
	selectedIndex := self widget selectedIndex.
	
	selectedIndex = 0 ifTrue: [ ^ nil ].
	
	^ (self widget dataSource elementAt: selectedIndex) data
]

{ #category : #accessing }
MorphicTreeTableAdapter >> selectedPaths [
	
	^ self widget selectedIndexes collect: [ :e | self widget dataSource pathFromIndex: e  ] 
]

{ #category : #factory }
MorphicTreeTableAdapter >> selectionChanged: ann [

	self suspendSelectionEventsDuring: [ 
		self basicSelectionChanged: ann ]
]

{ #category : #factory }
MorphicTreeTableAdapter >> strongSelectionChanged: aFTStrongSelectionChanged [

	self presenter activatesOnDoubleClick ifFalse: [ ^ self ].	
	self presenter doubleClickAtIndex: aFTStrongSelectionChanged selectedIndex
]

{ #category : #factory }
MorphicTreeTableAdapter >> suspendSelectionEventsDuring: aBlock [
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

{ #category : #selection }
MorphicTreeTableAdapter >> updateSelectionOf: tableMorph [
	
	self selectInto: tableMorph paths: self model selection selectedPaths
]
