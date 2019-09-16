"
I am the Spec presenter of a MorphTreeMorph tree.
"
Class {
	#name : #SpTreePresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#instVars : [
		'childrenBlockHolder',
		'menuBlockHolder',
		'rootsHolder',
		'selectionHolder',
		'autoMultiSelection',
		'columnInset',
		'doubleClick',
		'evenRowColor',
		'isCheckList',
		'keyStroke',
		'multiSelection',
		'oddRowColor',
		'preferedPaneColor',
		'resizerWidth',
		'rowInset',
		'displayBlock',
		'columns',
		'treeUpdated',
		'iconBlock',
		'autoDeselection',
		'allowMenuOnNoItem',
		'rootNodeHolder',
		'removeOnlyLastSelected',
		'highlightedItem',
		'hasChildrenBlockHolder',
		'autoRefreshOnExpand'
	],
	#category : #'Spec2-Deprecated-Replaced'
}

{ #category : #specs }
SpTreePresenter class >> adapterName [
	^ #TreeAdapter
]

{ #category : #examples }
SpTreePresenter class >> example [
	<sampleInstance>
	| m |

	m := self new.
	m openWithSpec.
	
	m roots: #(a b c d).
	m childrenBlock: [:it | it = #c 
		ifTrue: [ {1. 2. 3} ]
		ifFalse: [ {} ]].
	m displayBlock: [:item | item isSymbol
		ifTrue: [ item asUppercase , ' symbol' ]
		ifFalse: [ item printString ]].
	m 
		dragEnabled: true;
		dropEnabled: true;
		wantDropBlock: [ :draggedItem :event :source | 
			draggedItem isTransferable and: [ draggedItem source = m ] ];
		acceptDropBlock: [ :transfer :event :source :receiver | 
			Transcript 
				show: 'Dragged node '; 
				show: transfer passenger content; 
				show: ' to node '; 
				show: receiver content; 
				cr ].
		
	^ m
]

{ #category : #examples }
SpTreePresenter class >> exampleOfAutoRefreshOnExpand [
	"Each time a node is expanded, the computation of the children is made again, instead of being cached as it is usually"

	SpTreePresenter new
		childrenBlock: [ :number | 
			self logCr: 'Compute children of ', number asString.
			1 to: number ];
		hasChildrenBlock: [ :number | true ];
		roots: (1 to: 10);
		autoRefreshOnExpand: true;
		openWithSpec
]

{ #category : #examples }
SpTreePresenter class >> exampleWithCustomColumnsAndNodes [
	<haltOrBreakpointForTesting>
	| m col1 col2 |

	m := SpTreePresenter new.
	m roots: #(#a #b #c #d).
	m rootNodeHolder: [ :item | 
		SpTreeNodePresenter new
			content: item;
			icon: (self iconNamed: #smallConfigurationIcon)
	].
	
	m openWithSpec.
	
	col1 := SpTreeColumnPresenter new
		displayBlock: [ :node | node content asString ];
		headerLabel: 'Character'.
	col2 := SpTreeColumnPresenter new
		displayBlock: [ :node | (Character value: node content asString first asciiValue + 1) asString ];
		headerLabel: 'Character +1';
		headerIcon: (self iconNamed: #smallBackIcon).
	
	m
		columns: {col1. col2};
		dragEnabled: true;
		dropEnabled: true;
		acceptDropBlock: [ :transfer :event :source :receiver | self halt ].
		
	col2 
		headerLabel: 'Character +2';
		headerIcon: (self iconNamed: #smallBackIcon);
		displayBlock: [ :node | (Character value: node content asString first asciiValue + 2) asString ].
	
	m rootNodeHolder: [ :item | 
		SpTreeNodePresenter new
			content: (Character value: (item asString first asciiValue + 5)) asSymbol;
			icon: (self iconNamed: #smallFullscreenIcon)
	].

	col1 headerIcon: (self iconNamed: #nautilusIcon).
]

{ #category : #examples }
SpTreePresenter class >> exampleWithCustomColumnsAndNodesAndChildren [
	<haltOrBreakpointForTesting>

	| m col1 col2 |
	m := SpTreePresenter new.
	m roots: #(#a #b #c #d).
	m
		rootNodeHolder: [ :item | 
			SpTreeNodePresenter new
				content: item;
				children: [ (1 to: item asString first asciiValue) collect: [ :e | SpTreeNodePresenter new content: e ] ];
				icon: (m iconNamed: #smallConfigurationIcon) ].
	m openWithSpec.
	col1 := SpTreeColumnPresenter new
		displayBlock: [ :node | node content asString ];
		headerAction: [ self halt ].
	col2 := SpTreeColumnPresenter new
		displayBlock: [ :node | (Character value: node content asString first asciiValue + 1) asString ].
	m
		columns:
			{col1.
			col2};
		dragEnabled: true;
		dropEnabled: true;
		acceptDropBlock: [ :transfer :event :source :receiver | self halt ].
	col2
		displayBlock: [ :node | (Character value: node content asString first asciiValue + 2) asString ]
]

{ #category : #examples }
SpTreePresenter class >> exampleWithNoSpecifiedNodes [
	"self exampleWithNoSpecifiedNodes"

	SpTreePresenter new
		 roots: (1 to: 5);
		 childrenBlock: [ :item | 1+item to: 5+item ];
		 openWithSpec
]

{ #category : #specs }
SpTreePresenter class >> isDeprecated [ 

	^ true
]

{ #category : #api }
SpTreePresenter >> allowMenuOnNoItem [

	^ allowMenuOnNoItem value
]

{ #category : #api }
SpTreePresenter >> allowMenuOnNoItem: aBoolean [

	allowMenuOnNoItem value: aBoolean
]

{ #category : #api }
SpTreePresenter >> autoDeselection [

	^ autoDeselection value
]

{ #category : #api }
SpTreePresenter >> autoDeselection: aBoolean [

	autoDeselection value: aBoolean
]

{ #category : #api }
SpTreePresenter >> autoMultiSelection [

	^ autoMultiSelection value
]

{ #category : #api }
SpTreePresenter >> autoMultiSelection: aBoolean [

	autoMultiSelection value: aBoolean
]

{ #category : #api }
SpTreePresenter >> autoRefreshOnExpand [
	^ autoRefreshOnExpand value
]

{ #category : #api }
SpTreePresenter >> autoRefreshOnExpand: aBoolean [
	autoRefreshOnExpand value: aBoolean
]

{ #category : #api }
SpTreePresenter >> beCheckList [

	self isCheckList: true
]

{ #category : #'api-compatibility' }
SpTreePresenter >> childrenBlock [
	"Return the block used to retrieve the children of a node.
	The optional block arguments are:
		- the node
		- the tree"

	^ childrenBlockHolder value
]

{ #category : #'api-compatibility' }
SpTreePresenter >> childrenBlock: aBlock [
	"Set the block used to retrieve the children of a node.
	The optional block arguments are:
		- the node
		- the tree"

	childrenBlockHolder value: aBlock
]

{ #category : #api }
SpTreePresenter >> childrenFor: anItem [

	^ self childrenBlock cull: anItem cull: self
]

{ #category : #api }
SpTreePresenter >> collapseAll [
	
	self changed: #collapseAll with: #()
]

{ #category : #api }
SpTreePresenter >> columnInset [

	^ columnInset value
]

{ #category : #api }
SpTreePresenter >> columnInset: anInteger [

	columnInset value: anInteger
]

{ #category : #api }
SpTreePresenter >> columns [
	
	^ columns value
]

{ #category : #api }
SpTreePresenter >> columns: aCollection [
	
	columns value: aCollection
]

{ #category : #initialization }
SpTreePresenter >> defaultNodeFor: item [

	^ SpTreeNodePresenter new
		content: item;
		children: [ self childrenFor: item ];
		hasChildren: [ self hasChildrenFor: item ];
		yourself
]

{ #category : #api }
SpTreePresenter >> deselectAll [
	
	self changed: #deselectAll with: #()
]

{ #category : #'api-compatibility' }
SpTreePresenter >> displayBlock [
	"Return the block used to generate the display of the items.
	The optioanl block arguments are:
		- the item
		- the tree"

	^ displayBlock value
]

{ #category : #'api-compatibility' }
SpTreePresenter >> displayBlock: aBlock [
	"Set the block used to generate the display of the items.
	The optioanl block arguments are:
		- the item
		- the tree"

	displayBlock value: aBlock
]

{ #category : #api }
SpTreePresenter >> doubleClick [

	doubleClick value value
]

{ #category : #api }
SpTreePresenter >> doubleClick: aBlock [

	doubleClick value: aBlock
]

{ #category : #api }
SpTreePresenter >> evenRowColor [

	^ evenRowColor value
]

{ #category : #api }
SpTreePresenter >> evenRowColor: aColor [

	evenRowColor value: aColor
]

{ #category : #focus }
SpTreePresenter >> eventKeyStrokesForNextFocus [
	"String describing the keystroke to perform to jump to the next widget"	
		
	^ { Character tab asKeyCombination }
]

{ #category : #focus }
SpTreePresenter >> eventKeyStrokesForPreviousFocus [
	"String describing the keystroke to perform to jump to the previous widget"
	
	^ { Character tab shift asKeyCombination }
]

{ #category : #api }
SpTreePresenter >> expandAll [

	self changed: #expandAll with: #()
]

{ #category : #api }
SpTreePresenter >> expandRoots [

	self changed: #expandRoots with: #()
]

{ #category : #'api-filtering' }
SpTreePresenter >> filterWith: aFilter [
	
	self selection: nil.
	self updateTree.

	self expandAll.
	self changed: #filterWith: with: {aFilter}.
	
	self expandAll.
]

{ #category : #'api-compatibility' }
SpTreePresenter >> hasChildrenBlock [
	"Get the block used to check if a node has children.
	The optional block arguments are:
		- the node
		- the tree"

	^ hasChildrenBlockHolder value
]

{ #category : #'api-compatibility' }
SpTreePresenter >> hasChildrenBlock: aBlock [
	"Set the block used to  check if a node has children.
	The optional block arguments are:
		- the node
		- the tree"

	hasChildrenBlockHolder value: aBlock
]

{ #category : #api }
SpTreePresenter >> hasChildrenFor: anItem [

	^ self hasChildrenBlock cull: anItem cull: self
]

{ #category : #api }
SpTreePresenter >> hasSelectedItem [
	"Return true if the tree contains any selected items"
	
	^ selectionHolder value isNotEmpty
]

{ #category : #api }
SpTreePresenter >> highlightedItem [
	"Return the currently highlighted item"

	^ highlightedItem value
]

{ #category : #api }
SpTreePresenter >> highlightedItem: anItem [
	"Set the highlightedItem item"
	"In the case of keyboard navigation with checklist, it may differ from the last selected item"
	
	highlightedItem value: anItem
]

{ #category : #api }
SpTreePresenter >> iconBlock: aBlock [

	iconBlock value: aBlock
]

{ #category : #api }
SpTreePresenter >> iconFor: aNode [

	^ iconBlock value cull: aNode content cull: aNode
]

{ #category : #initialization }
SpTreePresenter >> initialize [
	super initialize.

	childrenBlockHolder := [ {} ] asValueHolder.
	hasChildrenBlockHolder := [ :item | (self childrenFor: item) notEmpty ] asValueHolder.
	rootsHolder := {} asValueHolder.
	selectionHolder := {} asValueHolder.
	menuBlockHolder := [] asValueHolder.
	displayBlock := [:item | item asString ] asValueHolder.
	
	removeOnlyLastSelected := false asValueHolder.
	autoDeselection := true asValueHolder.
	autoMultiSelection := false asValueHolder.
	columnInset := 5 asValueHolder.
	doubleClick := [] asValueHolder.
	allowMenuOnNoItem := false asValueHolder.
	isCheckList := false asValueHolder.
	keyStroke := [ :key | ] asValueHolder.
	multiSelection := false asValueHolder.
	evenRowColor := self theme backgroundColor asValueHolder.
	oddRowColor := self theme backgroundColor asValueHolder.
	preferedPaneColor := self theme backgroundColor asValueHolder.
	resizerWidth := 2 asValueHolder.
	rowInset := 2 asValueHolder.
	iconBlock := [ :each : node | node icon ] asValueHolder.
	highlightedItem := nil asValueHolder.
	rootNodeHolder := [ :item |
		SpTreeNodePresenter new 
			content: item;
			children: [ self childrenFor: item ] ] asValueHolder.
	
	columns := { SpTreeColumnPresenter new
		displayBlock: [:node | (self wrapItem: node content) ] } asValueHolder.
	
	autoRefreshOnExpand := false asValueHolder.
	
	"Since my nodes are already morphs, dragging them will actually extract them from their owner"
	self dragTransformationBlock: [ :item :source | item complexContents ].

	treeUpdated := nil asValueHolder.

	self registerEvents
]

{ #category : #api }
SpTreePresenter >> isCheckList [

	^ isCheckList value
]

{ #category : #api }
SpTreePresenter >> isCheckList: aBoolean [

	isCheckList value: aBoolean
]

{ #category : #api }
SpTreePresenter >> keyStroke: aKeyStroke [

	keyStroke value cull: aKeyStroke
]

{ #category : #api }
SpTreePresenter >> keyStrokeAction: aBlock [

	keyStroke value: aBlock
]

{ #category : #api }
SpTreePresenter >> menu [
	"Return the block used to generate the tree menu"

	^ menuBlockHolder value
]

{ #category : #api }
SpTreePresenter >> menu: aBlock [
	"Set the block used to generate the tree menu"

	menuBlockHolder value: aBlock
]

{ #category : #api }
SpTreePresenter >> menu: aMenu shifted: aBoolean [
	| menu |
	
	(self allowMenuOnNoItem not and: [ self selectedItems isEmpty ])
		ifTrue: [ ^ nil ].
		
	
	menuBlockHolder value ifNil: [ ^ nil ].
	menuBlockHolder value isBlock ifFalse: [ ^ menuBlockHolder value ].
	
	menu := SpMenuPresenter new.
	
	menuBlockHolder value cull: menu cull: aBoolean.
	
	^ menu
]

{ #category : #api }
SpTreePresenter >> multiSelection [

	^ multiSelection value
]

{ #category : #api }
SpTreePresenter >> multiSelection: aBoolean [

	multiSelection value: aBoolean
]

{ #category : #api }
SpTreePresenter >> oddRowColor [

	^ oddRowColor value
]

{ #category : #api }
SpTreePresenter >> oddRowColor: aColor [

	oddRowColor value: aColor
]

{ #category : #initialization }
SpTreePresenter >> onWidgetBuilt [
	^ self selectedItems: selectionHolder value
]

{ #category : #api }
SpTreePresenter >> preferedPaneColor [

	^ preferedPaneColor value
]

{ #category : #api }
SpTreePresenter >> preferedPaneColor: aColor [

	preferedPaneColor value: aColor
]

{ #category : #initialization }
SpTreePresenter >> registerEvents [
	
	rootsHolder whenChangedDo: [ self updateTree ].
	childrenBlockHolder whenChangedDo: [ self updateTree ].
	hasChildrenBlockHolder whenChangedDo: [ self updateTree ].
	displayBlock whenChangedDo: [ self updateTree ].
	
	iconBlock whenChangedDo: [ self updateTree ].
	autoDeselection whenChangedDo: [ :boolean | self update: {#autoDeselection:. boolean} ].
	columns whenChangedDo: [ :col | 
		col do: [ :each | each container: self ].
		self changed: #columns: with: { col } ].
	selectionHolder whenChangedDo: [:items | self changed: { #selectItems. items } ].
	rootNodeHolder whenChangedDo: [ :aBlock | self updateTree ].
	isCheckList whenChangedDo: [ :boolean | self changed: #isCheckList: with: { boolean }].
	oddRowColor whenChangedDo: [ :aColor | self changed: #oddRowColor: with: { aColor } ].
	evenRowColor whenChangedDo: [ :aColor | self changed: #evenRowColor: with: { aColor } ].
	
	menuBlockHolder  whenChangedDo: [ :aMenuModel :oldMenuModel |
		oldMenuModel ifNotNil: [ | oldMenu |
			oldMenuModel isBlock
				ifTrue: [ 
					oldMenu := SpMenuPresenter new.
					oldMenuModel cull: oldMenu cull: false ]
				ifFalse: [ oldMenu := oldMenuModel ].
		oldMenu applyTo: self ].
		
		aMenuModel ifNotNil: [ | newMenu |
			aMenuModel isBlock
				ifTrue: [ 
					newMenu := SpMenuPresenter new.
					aMenuModel cull: newMenu cull: false ]
				ifFalse: [ newMenu := aMenuModel ].
		newMenu applyTo: self ] ].

	self announcer 
		when: SpWidgetBuilt
		send: #onWidgetBuilt
		to: self.
]

{ #category : #api }
SpTreePresenter >> removeOnlyLastSelected: aBoolean [

	removeOnlyLastSelected value: aBoolean
]

{ #category : #api }
SpTreePresenter >> resetSelection [

	self selectedItems: {}
]

{ #category : #api }
SpTreePresenter >> resizerWidth [

	^ resizerWidth value
]

{ #category : #api }
SpTreePresenter >> resizerWidth: anInteger [

	resizerWidth value: anInteger
]

{ #category : #api }
SpTreePresenter >> rootNodeFor: item [
	| node |
	
	node := self rootNodeHolder cull: item.
	node container: self.
	
	^ node
]

{ #category : #api }
SpTreePresenter >> rootNodeHolder [

	^ rootNodeHolder value
]

{ #category : #api }
SpTreePresenter >> rootNodeHolder: aBlock [

	rootNodeHolder value: aBlock
]

{ #category : #api }
SpTreePresenter >> roots [
	"Return the tree roots"

	^ rootsHolder value
]

{ #category : #api }
SpTreePresenter >> roots: aCollection [
	"Set the tree roots"

	rootsHolder value: aCollection.
	self changed: #setRoots: with: { aCollection }
]

{ #category : #api }
SpTreePresenter >> rowInset [

	^ rowInset value
]

{ #category : #api }
SpTreePresenter >> rowInset: anInteger [

	rowInset value: anInteger
]

{ #category : #api }
SpTreePresenter >> secondSelection: anItem [
	
	self widget ifNotNil: [ :w | w listManager secondSelection: anItem ]
]

{ #category : #api }
SpTreePresenter >> selectAll [
	
	self changed: #selectAdd with: #()
]

{ #category : #api }
SpTreePresenter >> selectOnlyLastHighlighted [

	^ removeOnlyLastSelected value
]

{ #category : #api }
SpTreePresenter >> selectedItem [
	"Return the selected item"
	"In the case of a multiple selection list, it returns the last selected item"
	
	^ selectionHolder value
		ifEmpty: [ nil ]
		ifNotEmpty: [ :col | col  last ]
]

{ #category : #api }
SpTreePresenter >> selectedItem: anItem [
	"Set the selected item"
	"In the case of a multiple selection list, it returns the last selected item"
	
	self selectedItems: { anItem }
]

{ #category : #api }
SpTreePresenter >> selectedItems [
	"Return the selected items"
	
	^ selectionHolder value
]

{ #category : #api }
SpTreePresenter >> selectedItems: aCollection [
	"Set the selected item"
	"In the case of a multiple selection list, it returns the last selected item"

	selectionHolder value: aCollection.
	
]

{ #category : #api }
SpTreePresenter >> selection: aSelection [
 
	self changed: #selection: with: { aSelection }
]

{ #category : #api }
SpTreePresenter >> selectionChanged [
	
	self changed: #selectionChanged with: #()
]

{ #category : #initialization }
SpTreePresenter >> selectionChanged: ann [

	^ selectionHolder value: (ann selection
		ifNil: [ #() ]
		ifNotNil: [:selection | selection selectedNodes ])
]

{ #category : #api }
SpTreePresenter >> updateTree [
	"Force the tree to refresh"

	self changed: #updateTree with: {}.
	treeUpdated value: nil.
]

{ #category : #'as yet unclassified' }
SpTreePresenter >> whenChildrenBlockChanged: aBlock [
	self deprecated: 'Use #whenChildrenBlockChangedDo: instead.' transformWith: '`@receiver whenChildrenBlockChanged: `@statements' -> '`@receiver whenChildrenBlockChangedDo: `@statements'.
	self whenChildrenBlockChangedDo: aBlock
]

{ #category : #'api-events-compatibility' }
SpTreePresenter >> whenChildrenBlockChangedDo: aBlock [
	"Set a block to perform when a new childrenBlock is set"

	childrenBlockHolder whenChangedDo: aBlock
]

{ #category : #'as yet unclassified' }
SpTreePresenter >> whenDisplayBlockChanged: aBlock [
	self deprecated: 'Use #whenDisplayBlockChangedDo: instead.' transformWith: '`@receiver whenDisplayBlockChanged: `@statements' -> '`@receiver whenDisplayBlockChangedDo: `@statements'.
	self whenDisplayBlockChangedDo: aBlock
]

{ #category : #'api-events-compatibility' }
SpTreePresenter >> whenDisplayBlockChangedDo: aBlock [
	"Set a block to perform when a new displayBlock is set"

	displayBlock whenChangedDo: aBlock
]

{ #category : #'as yet unclassified' }
SpTreePresenter >> whenHasChildrenBlockChanged: aBlock [
	self deprecated: 'Use #whenHasChildrenBlockChangedDo: instead.' transformWith: '`@receiver whenHasChildrenBlockChanged: `@statements' -> '`@receiver whenHasChildrenBlockChangedDo: `@statements'.
	self whenHasChildrenBlockChangedDo: aBlock
]

{ #category : #'api-events-compatibility' }
SpTreePresenter >> whenHasChildrenBlockChangedDo: aBlock [
	"Set a block to perform when a new hasChildrenBlock is set"

	hasChildrenBlockHolder whenChangedDo: aBlock
]

{ #category : #'as yet unclassified' }
SpTreePresenter >> whenHighlightedItemChanged: aBlock [
	self deprecated: 'Use #whenHighlightedItemChangedDo: instead.' transformWith: '`@receiver whenHighlightedItemChanged: `@statements' -> '`@receiver whenHighlightedItemChangedDo: `@statements'.
	self whenHighlightedItemChangedDo: aBlock
]

{ #category : #'api-events' }
SpTreePresenter >> whenHighlightedItemChangedDo: aBlock [

	highlightedItem whenChangedDo: aBlock
]

{ #category : #'as yet unclassified' }
SpTreePresenter >> whenMenuChanged: aBlock [
	self deprecated: 'Use #whenMenuChangedDo: instead.' transformWith: '`@receiver whenMenuChanged: `@statements' -> '`@receiver whenMenuChangedDo: `@statements'.
	self whenMenuChangedDo: aBlock
]

{ #category : #'api-events' }
SpTreePresenter >> whenMenuChangedDo: aBlock [
	"Set a block to perform when a new menuBlock is set"

	menuBlockHolder whenChangedDo: aBlock
]

{ #category : #'as yet unclassified' }
SpTreePresenter >> whenRootsChanged: aBlock [
	self deprecated: 'Use #whenRootsChangedDo: instead.' transformWith: '`@receiver whenRootsChanged: `@statements' -> '`@receiver whenRootsChangedDo: `@statements'.
	self whenRootsChangedDo: aBlock
]

{ #category : #'api-events' }
SpTreePresenter >> whenRootsChangedDo: aBlock [

	rootsHolder whenChangedDo: aBlock
]

{ #category : #'as yet unclassified' }
SpTreePresenter >> whenSelectedItemChanged: aBlock [
	self deprecated: 'Use #whenSelectedItemChangedDo: instead.' transformWith: '`@receiver whenSelectedItemChanged: `@statements' -> '`@receiver whenSelectedItemChangedDo: `@statements'.
	self whenSelectedItemChangedDo: aBlock
]

{ #category : #'api-events-compatibility' }
SpTreePresenter >> whenSelectedItemChangedDo: aBlock [
	"Set a block to perform when a new item is selected"

	selectionHolder whenChangedDo: [ :col | 
		col 
			ifNil: [ aBlock value: nil ]
			ifNotNil: [ col isEmpty
					ifTrue: [ aBlock value: nil ]
					ifFalse: [ aBlock value: (col last lastClicked
						ifTrue: [ col last content ]
						ifFalse: [ nil ]) ] ] ]
]

{ #category : #'as yet unclassified' }
SpTreePresenter >> whenSelectedItemsChanged: aBlock [
	self deprecated: 'Use #whenSelectedItemsChangedDo: instead.' transformWith: '`@receiver whenSelectedItemsChanged: `@statements' -> '`@receiver whenSelectedItemsChangedDo: `@statements'.
	self whenSelectedItemsChangedDo: aBlock
]

{ #category : #'api-events' }
SpTreePresenter >> whenSelectedItemsChangedDo: aBlock [
	"Set a block to perform when a new item is selected"

	selectionHolder whenChangedDo: [ :col | aBlock cull: (col collect: [:e | e content ]) ]
]

{ #category : #'as yet unclassified' }
SpTreePresenter >> whenTreeUpdated: aBlock [
	self deprecated: 'Use #whenTreeUpdatedDo: instead.' transformWith: '`@receiver whenTreeUpdated: `@statements' -> '`@receiver whenTreeUpdatedDo: `@statements'.
	self whenTreeUpdatedDo: aBlock
]

{ #category : #'api-events' }
SpTreePresenter >> whenTreeUpdatedDo: aBlock [

	treeUpdated whenChangedDo: aBlock
]

{ #category : #api }
SpTreePresenter >> wrapItem: anItem [

	^ displayBlock value cull: anItem cull: self
]
