"
I am the adapter used to bridge a TreePresenter and a MorphTreeMorph
"
Class {
	#name : #MorphicTreeAdapter,
	#superclass : #AbstractMorphicAdapter,
	#instVars : [
		'treeModel'
	],
	#category : #'Spec-MorphicAdapters'
}

{ #category : #'drag and drop - private' }
MorphicTreeAdapter >> acceptDroppingMorph: draggedMorph event: event inMorph: source [
	| item |
	item := self widget ifNotNil: [ :w | w scrollerSubMorphFromPoint: event position ].
	item ifNil: [ ^ self ].
	^ self acceptDropBlock
		valueWithEnoughArguments: { draggedMorph model model. event. source model model model. item model nodeModel}
]

{ #category : #'widget API' }
MorphicTreeAdapter >> autoDeselection [

	^ self model autoDeselection
]

{ #category : #'widget API' }
MorphicTreeAdapter >> autoMultiSelection [

	^ self model autoMultiSelection
]

{ #category : #factory }
MorphicTreeAdapter >> buildWidget [
	^ MorphTreeMorph new
		model: self treeModel;
		dragEnabled: self dragEnabled;
		dropEnabled: self dropEnabled;
		initializeColumsFrom: self;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		autoDeselection: self autoDeselection;
		resizerWidth: self resizerWidth;
		columnInset: self columnInset;
		iconBlock: self iconBlock;
		rowInset: self rowInset;
		keystrokeActionSelector: #keyStroke:;
		multiSelection: self multiSelection;
		autoMultiSelection: self autoMultiSelection;
		removeOnlyLastSelected: self selectOnlyLastHighlighted;
		isCheckList: self isCheckList;
		doubleClickSelector: #doubleClick;
		getMenuSelector: #menu:shifted:;
		rowColorForEven: self evenRowColor;
		rowColorForOdd: self oddRowColor;
		buildContents;
		yourself
]

{ #category : #updating }
MorphicTreeAdapter >> changed: aSymbol [

	treeModel changed: aSymbol
]

{ #category : #'widget API' }
MorphicTreeAdapter >> childrenFor: aNode [

	^ self model childrenFor: aNode
]

{ #category : #'spec protocol' }
MorphicTreeAdapter >> collapseAll [

	self treeModel collapseAll
]

{ #category : #'widget API' }
MorphicTreeAdapter >> columnInset [

	^ self model columnInset
]

{ #category : #'widget API' }
MorphicTreeAdapter >> columns [

	^ self model columns collect: [ :each | each buildWithSpec ]
]

{ #category : #'spec protocol' }
MorphicTreeAdapter >> columns: newColumns [

	self widgetDo: [ :w || columns |
		columns := newColumns collect: [ :each | each buildWithSpec ]. 
		w columns: columns.
		w resizerChanged.
		w updateList ]
]

{ #category : #'reflective operations' }
MorphicTreeAdapter >> doesNotUnderstand: aMessage [

	(self treeModel respondsTo: aMessage selector) 
		ifFalse: [ ^ super doesNotUnderstand: aMessage ].
		
	^ self treeModel 
		perform: aMessage selector 
		withArguments: aMessage arguments
]

{ #category : #'widget API' }
MorphicTreeAdapter >> doubleClick [

	^ self model doubleClick
]

{ #category : #'widget API' }
MorphicTreeAdapter >> evenRowColor [

	^ self model evenRowColor
]

{ #category : #'widget API' }
MorphicTreeAdapter >> evenRowColor: aColor [

	^ self widget rowColorForEven: aColor
]

{ #category : #'spec protocol' }
MorphicTreeAdapter >> expandAll [

	self treeModel expandAll
]

{ #category : #'spec protocol' }
MorphicTreeAdapter >> expandRoots [

	self treeModel expandRoots
]

{ #category : #'spec protocol' }
MorphicTreeAdapter >> filterWith: aFilter [
	
	self widgetDo: [ :w || nodes |
		nodes := w model rootNodes.
		nodes do: [:r | r nodeModel updateAccordingTo: aFilter].
	
		self removeRootsSuchAs: [:n | (aFilter keepTreeNode: n) not and: [n isEmpty]].

		self changed: #rootNodes ].
]

{ #category : #'widget API' }
MorphicTreeAdapter >> iconBlock [

	^ [:each | self iconFor: each ]
]

{ #category : #'widget API' }
MorphicTreeAdapter >> iconFor: aNode [

	^ self model iconFor: aNode
]

{ #category : #initialize }
MorphicTreeAdapter >> initialize [

	super initialize.

	treeModel := SpecTreeModel model: self.	
	treeModel announcer
		when: MorphTreeSelectionChanged
		send: #onMorphTreeSelectionChanged:
		to: self.
]

{ #category : #'widget API' }
MorphicTreeAdapter >> isCheckList [

	^ self model isCheckList
]

{ #category : #'spec protocol' }
MorphicTreeAdapter >> isCheckList: aBoolean [

	self widgetDo: [ :w | w isCheckList: aBoolean ]
]

{ #category : #'widget API' }
MorphicTreeAdapter >> keyStroke: aKeyStroke [

	^ self model keyStroke: aKeyStroke
]

{ #category : #'widget API' }
MorphicTreeAdapter >> menu: aMenu shifted: aBoolean [

	^ (self model menu: aMenu shifted: aBoolean) 
		ifNotNil: [ :menuModel | menuModel buildWithSpecAsPopup ]
]

{ #category : #'widget API' }
MorphicTreeAdapter >> multiSelection [

	^ self model multiSelection
]

{ #category : #'widget API' }
MorphicTreeAdapter >> oddRowColor [

	^ self model oddRowColor
]

{ #category : #'widget API' }
MorphicTreeAdapter >> oddRowColor: aColor [

	^ self widget rowColorForOdd: aColor
]

{ #category : #'spec protocol' }
MorphicTreeAdapter >> onMorphTreeSelectionChanged: ann [
	| items |
	
	items := (ann selection
		ifNil: [ #() ]
		ifNotNil: [:selection | selection selectedNodes ]).
		
	items := items collect: [ :each | each nodeModel ].

	self model selectedItems: items
]

{ #category : #'widget API' }
MorphicTreeAdapter >> preferedPaneColor [

	^ self model preferedPaneColor
]

{ #category : #'spec protocol' }
MorphicTreeAdapter >> removeRootsSuchAs: aBlock [

	self widgetDo: [ :w | w model rootNodes removeAllSuchThat: [ :node | aBlock value: node nodeModel ] ]
]

{ #category : #'widget API' }
MorphicTreeAdapter >> resizerWidth [

	^ self model resizerWidth
]

{ #category : #'widget API' }
MorphicTreeAdapter >> rootNodeFromItem: item [
	| node |
	
	node := item isTreeNodeModel
		ifTrue: [ item ]
		ifFalse: [ self rootNodeHolder cull: item ].
		
	node container: self model.
	^ node buildWithSpec
]

{ #category : #accessing }
MorphicTreeAdapter >> rootNodeHolder [

	^ self model rootNodeHolder
]

{ #category : #'widget API' }
MorphicTreeAdapter >> roots [

	^ self model roots
]

{ #category : #'widget API' }
MorphicTreeAdapter >> rowInset [

	^ self model rowInset
]

{ #category : #'spec protocol' }
MorphicTreeAdapter >> selectAdd [

	self widgetDo: [ :w | w selectAll ]
]

{ #category : #'widget API' }
MorphicTreeAdapter >> selectOnlyLastHighlighted [

	^ self model selectOnlyLastHighlighted
]

{ #category : #'spec protocol' }
MorphicTreeAdapter >> setRoots: aCollection [

	treeModel rootItems: aCollection
]

{ #category : #accessing }
MorphicTreeAdapter >> treeModel [
	^ treeModel
]

{ #category : #protocol }
MorphicTreeAdapter >> updateTree [

	self treeModel resetRootNodes.
	self treeModel changed: #rootNodes.
]

{ #category : #'widget API' }
MorphicTreeAdapter >> wrapItem: anItem [

	^ self model wrapItem: anItem
]
