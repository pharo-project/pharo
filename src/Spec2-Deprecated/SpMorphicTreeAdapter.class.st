"
I am the adapter used to bridge a TreePresenter and a MorphTreeMorph
"
Class {
	#name : #SpMorphicTreeAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#instVars : [
		'treeModel'
	],
	#category : #'Spec2-Deprecated-Adapters'
}

{ #category : #deprecation }
SpMorphicTreeAdapter class >> isDeprecated [ 

	^ true
]

{ #category : #'drag and drop - private' }
SpMorphicTreeAdapter >> acceptDroppingMorph: draggedMorph event: event inMorph: source [
	| item |
	item := self widget ifNotNil: [ :w | w scrollerSubMorphFromPoint: event position ].
	item ifNil: [ ^ self ].
	^ self acceptDropBlock
		valueWithEnoughArguments: { draggedMorph model model. event. source model model model. item model nodeModel}
]

{ #category : #'widget API' }
SpMorphicTreeAdapter >> autoDeselection [

	^ self model autoDeselection
]

{ #category : #'widget API' }
SpMorphicTreeAdapter >> autoMultiSelection [

	^ self model autoMultiSelection
]

{ #category : #factory }
SpMorphicTreeAdapter >> buildWidget [
	^ MorphTreeMorph new
		model: self treeModel;
		dragEnabled: self dragEnabled;
		dropEnabled: self dropEnabled;
		spInitializeColumsFrom: self;
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
SpMorphicTreeAdapter >> changed: aSymbol [

	treeModel changed: aSymbol
]

{ #category : #'widget API' }
SpMorphicTreeAdapter >> childrenFor: aNode [

	^ self model childrenFor: aNode
]

{ #category : #'spec protocol' }
SpMorphicTreeAdapter >> collapseAll [

	self treeModel collapseAll
]

{ #category : #'widget API' }
SpMorphicTreeAdapter >> columnInset [

	^ self model columnInset
]

{ #category : #'widget API' }
SpMorphicTreeAdapter >> columns [

	^ self model columns collect: [ :each | each buildWithSpec ]
]

{ #category : #'spec protocol' }
SpMorphicTreeAdapter >> columns: newColumns [

	self widgetDo: [ :w || columns |
		columns := newColumns collect: [ :each | each buildWithSpec ]. 
		w columns: columns.
		w resizerChanged.
		w updateList ]
]

{ #category : #'reflective operations' }
SpMorphicTreeAdapter >> doesNotUnderstand: aMessage [

	(self treeModel respondsTo: aMessage selector) 
		ifFalse: [ ^ super doesNotUnderstand: aMessage ].
		
	^ self treeModel 
		perform: aMessage selector 
		withArguments: aMessage arguments
]

{ #category : #'widget API' }
SpMorphicTreeAdapter >> doubleClick [

	^ self model doubleClick
]

{ #category : #'widget API' }
SpMorphicTreeAdapter >> evenRowColor [

	^ self model evenRowColor
]

{ #category : #'widget API' }
SpMorphicTreeAdapter >> evenRowColor: aColor [

	^ self widget rowColorForEven: aColor
]

{ #category : #'spec protocol' }
SpMorphicTreeAdapter >> expandAll [

	self treeModel expandAll
]

{ #category : #'spec protocol' }
SpMorphicTreeAdapter >> expandRoots [

	self treeModel expandRoots
]

{ #category : #'spec protocol' }
SpMorphicTreeAdapter >> filterWith: aFilter [
	
	self widgetDo: [ :w || nodes |
		nodes := w model rootNodes.
		nodes do: [:r | r nodeModel updateAccordingTo: aFilter].
	
		self removeRootsSuchAs: [:n | (aFilter keepTreeNode: n) not and: [n isEmpty]].

		self changed: #rootNodes ].
]

{ #category : #'widget API' }
SpMorphicTreeAdapter >> iconBlock [

	^ [:each | self iconFor: each ]
]

{ #category : #'widget API' }
SpMorphicTreeAdapter >> iconFor: aNode [

	^ self model iconFor: aNode
]

{ #category : #initialization }
SpMorphicTreeAdapter >> initialize [

	super initialize.

	treeModel := SpTreeModel model: self.	
	treeModel announcer
		when: MorphTreeSelectionChanged
		send: #onMorphTreeSelectionChanged:
		to: self.
]

{ #category : #'widget API' }
SpMorphicTreeAdapter >> isCheckList [

	^ self model isCheckList
]

{ #category : #'spec protocol' }
SpMorphicTreeAdapter >> isCheckList: aBoolean [

	self widgetDo: [ :w | w isCheckList: aBoolean ]
]

{ #category : #'widget API' }
SpMorphicTreeAdapter >> keyStroke: aKeyStroke [

	^ self model keyStroke: aKeyStroke
]

{ #category : #'widget API' }
SpMorphicTreeAdapter >> menu: aMenu shifted: aBoolean [

	^ (self model menu: aMenu shifted: aBoolean) 
		ifNotNil: [ :menuModel | menuModel buildWithSpecAsPopup ]
]

{ #category : #'widget API' }
SpMorphicTreeAdapter >> multiSelection [

	^ self model multiSelection
]

{ #category : #'widget API' }
SpMorphicTreeAdapter >> oddRowColor [

	^ self model oddRowColor
]

{ #category : #'widget API' }
SpMorphicTreeAdapter >> oddRowColor: aColor [

	^ self widget rowColorForOdd: aColor
]

{ #category : #'spec protocol' }
SpMorphicTreeAdapter >> onMorphTreeSelectionChanged: ann [
	| items |
	
	items := (ann selection
		ifNil: [ #() ]
		ifNotNil: [:selection | selection selectedNodes ]).
		
	items := items collect: [ :each | each nodeModel ].

	self model selectedItems: items
]

{ #category : #'widget API' }
SpMorphicTreeAdapter >> preferedPaneColor [

	^ self model preferedPaneColor
]

{ #category : #'spec protocol' }
SpMorphicTreeAdapter >> removeRootsSuchAs: aBlock [

	self widgetDo: [ :w | w model rootNodes removeAllSuchThat: [ :node | aBlock value: node nodeModel ] ]
]

{ #category : #'widget API' }
SpMorphicTreeAdapter >> resizerWidth [

	^ self model resizerWidth
]

{ #category : #'widget API' }
SpMorphicTreeAdapter >> rootNodeFromItem: item [
	| node |
	
	node := (item isKindOf: SpTreeNodePresenter)
		ifTrue: [ item ]
		ifFalse: [ self rootNodeHolder cull: item ].
		
	node container: self model.
	^ node buildWithSpec
]

{ #category : #accessing }
SpMorphicTreeAdapter >> rootNodeHolder [

	^ self model rootNodeHolder
]

{ #category : #'widget API' }
SpMorphicTreeAdapter >> roots [

	^ self model roots
]

{ #category : #'widget API' }
SpMorphicTreeAdapter >> rowInset [

	^ self model rowInset
]

{ #category : #'spec protocol' }
SpMorphicTreeAdapter >> selectAdd [

	self widgetDo: [ :w | w selectAll ]
]

{ #category : #'widget API' }
SpMorphicTreeAdapter >> selectOnlyLastHighlighted [

	^ self model selectOnlyLastHighlighted
]

{ #category : #'spec protocol' }
SpMorphicTreeAdapter >> setRoots: aCollection [

	treeModel rootItems: aCollection
]

{ #category : #accessing }
SpMorphicTreeAdapter >> treeModel [
	^ treeModel
]

{ #category : #protocol }
SpMorphicTreeAdapter >> updateTree [

	self treeModel resetRootNodes.
	self treeModel changed: #rootNodes.
]

{ #category : #'widget API' }
SpMorphicTreeAdapter >> wrapItem: anItem [

	^ self model wrapItem: anItem
]
