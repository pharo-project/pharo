"
I am the Spec presenter of a MorphTreeMorph tree.
"
Class {
	#name : #TreePresenter,
	#superclass : #AbstractWidgetPresenter,
	#instVars : [
		'childrenBlockHolder',
		'menuBlockHolder',
		'rootsHolder',
		'selectionHolder',
		'selectedItem',
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
	#category : #'Spec-Core-Widgets'
}

{ #category : #specs }
TreePresenter class >> adapterName [
]

{ #category : #specs }
TreePresenter class >> defaultSpec [
	<spec: #default>
	
	^ #(TreeAdapter
		adapt: #(model))
]

{ #category : #examples }
TreePresenter class >> example [
	<sampleInstance>
	| m |
	m := self new.
	m openWithSpec.
	m roots: #(a b c d).
	m
		childrenBlock: [ :it | 
			it = #c
				ifTrue: [ {1 . 2 . 3} ]
				ifFalse: [ {} ] ].
	m
		displayBlock: [ :item | 
			item isSymbol
				ifTrue: [ item asUppercase , ' symbol' ]
				ifFalse: [ item printString ] ].
	m
		dragEnabled: true;
		dropEnabled: true;
		wantDropBlock:
				[ :draggedItem :event :source | draggedItem isTransferable and: [ draggedItem source = m ] ];
		acceptDropBlock: [ :transfer :event :source :receiver | 
			self
				traceCr:
					('Dragged node <1s> to node <2s>'
						expandMacrosWith: transfer passenger content asString
						with: receiver content asString) ].
	^ m
]

{ #category : #examples }
TreePresenter class >> exampleOfAutoRefreshOnExpand [
	"Each time a node is expanded, the computation of the children is made again, instead of being cached as it is usually"

	TreePresenter new
		childrenBlock: [ :number | 
			self logCr: 'Compute children of ', number asString.
			1 to: number ];
		hasChildrenBlock: [ :number | true ];
		roots: (1 to: 10);
		autoRefreshOnExpand: true;
		openWithSpec
]

{ #category : #examples }
TreePresenter class >> exampleWithCustomColumnsAndNodes [
	<haltOrBreakpointForTesting>
	"self exampleWithCustomColumnsAndNodes"
	| m col1 col2 |

	m := TreePresenter new.
	m roots: #(#a #b #c #d).
	m rootNodeHolder: [ :item | 
		TreeNodePresenter new
			content: item;
			icon: (self iconNamed: #smallConfigurationIcon)
	].
	
	m openWithSpec.
	
	col1 := TreeColumnPresenter new
		displayBlock: [ :node | node content asString ];
		headerLabel: 'Character'.
	col2 := TreeColumnPresenter new
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
		TreeNodePresenter new
			content: (Character value: (item asString first asciiValue + 5)) asSymbol;
			icon: (self iconNamed: #smallFullscreenIcon)
	].

	col1 headerIcon: (self iconNamed: #nautilusIcon).
]

{ #category : #examples }
TreePresenter class >> exampleWithCustomColumnsAndNodesAndChildren [
	<haltOrBreakpointForTesting>
	"self exampleWithCustomColumnsAndNodesAndChildren"

	| m col1 col2 |
	m := TreePresenter new.
	m roots: #(#a #b #c #d).
	m
		rootNodeHolder: [ :item | 
			TreeNodePresenter new
				content: item;
				children: [ (1 to: item asString first asciiValue) collect: [ :e | TreeNodePresenter new content: e ] ];
				icon: (m iconNamed: #smallConfigurationIcon) ].
	m openWithSpec.
	col1 := TreeColumnPresenter new
		displayBlock: [ :node | node content asString ];
		headerAction: [ self halt ].
	col2 := TreeColumnPresenter new
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
TreePresenter class >> exampleWithNoSpecifiedNodes [
	"self exampleWithNoSpecifiedNodes"

	TreePresenter new
		 roots: (1 to: 5);
		 childrenBlock: [ :item | 1+item to: 5+item ];
		 openWithSpec
]

{ #category : #api }
TreePresenter >> allowMenuOnNoItem [

	^ allowMenuOnNoItem value
]

{ #category : #api }
TreePresenter >> allowMenuOnNoItem: aBoolean [

	allowMenuOnNoItem value: aBoolean
]

{ #category : #api }
TreePresenter >> autoDeselection [

	^ autoDeselection value
]

{ #category : #api }
TreePresenter >> autoDeselection: aBoolean [

	autoDeselection value: aBoolean
]

{ #category : #api }
TreePresenter >> autoMultiSelection [

	^ autoMultiSelection value
]

{ #category : #api }
TreePresenter >> autoMultiSelection: aBoolean [

	autoMultiSelection value: aBoolean
]

{ #category : #api }
TreePresenter >> autoRefreshOnExpand [
	^ autoRefreshOnExpand value
]

{ #category : #api }
TreePresenter >> autoRefreshOnExpand: aBoolean [
	autoRefreshOnExpand value: aBoolean
]

{ #category : #api }
TreePresenter >> beCheckList [

	self isCheckList: true
]

{ #category : #'api-compatibility' }
TreePresenter >> childrenBlock [
	"< api: #inspect>"
	"Return the block used to retrieve the children of a node.
	The optional block arguments are:
		- the node
		- the tree"

	^ childrenBlockHolder value
]

{ #category : #'api-compatibility' }
TreePresenter >> childrenBlock: aBlock [
	"< api: #block getter: #childrenBlock registration: #whenChildrenBlockChanged:>"
	"Set the block used to retrieve the children of a node.
	The optional block arguments are:
		- the node
		- the tree"

	childrenBlockHolder value: aBlock
]

{ #category : #api }
TreePresenter >> childrenFor: anItem [

	^ self childrenBlock cull: anItem cull: self
]

{ #category : #api }
TreePresenter >> collapseAll [
	
	self changed: #collapseAll with: #()
]

{ #category : #api }
TreePresenter >> columnInset [

	^ columnInset value
]

{ #category : #api }
TreePresenter >> columnInset: anInteger [

	columnInset value: anInteger
]

{ #category : #api }
TreePresenter >> columns [
	
	^ columns value
]

{ #category : #api }
TreePresenter >> columns: aCollection [
	
	columns value: aCollection
]

{ #category : #initialization }
TreePresenter >> defaultNodeFor: item [

	^ TreeNodePresenter new
		content: item;
		children: [ self childrenFor: item ];
		hasChildren: [ self hasChildrenFor: item ];
		yourself
]

{ #category : #api }
TreePresenter >> deselectAll [
	
	self changed: #deselectAll with: #()
]

{ #category : #'api-compatibility' }
TreePresenter >> displayBlock [
	"< api: #inspect>"
	"Return the block used to generate the display of the items.
	The optioanl block arguments are:
		- the item
		- the tree"

	^ displayBlock value
]

{ #category : #'api-compatibility' }
TreePresenter >> displayBlock: aBlock [
	"<api: #block getter: #displayBlock registration: #whenDisplayBlockChanged:>"
	"Set the block used to generate the display of the items.
	The optioanl block arguments are:
		- the item
		- the tree"

	displayBlock value: aBlock
]

{ #category : #api }
TreePresenter >> doubleClick [

	doubleClick value value
]

{ #category : #api }
TreePresenter >> doubleClick: aBlock [

	doubleClick value: aBlock
]

{ #category : #api }
TreePresenter >> evenRowColor [

	^ evenRowColor value
]

{ #category : #api }
TreePresenter >> evenRowColor: aColor [

	evenRowColor value: aColor
]

{ #category : #focus }
TreePresenter >> eventKeyStrokesForNextFocus [
	"String describing the keystroke to perform to jump to the next widget"	
		
	^ { Character tab asKeyCombination }
]

{ #category : #focus }
TreePresenter >> eventKeyStrokesForPreviousFocus [
	"String describing the keystroke to perform to jump to the previous widget"
	
	^ { Character tab shift asKeyCombination }
]

{ #category : #api }
TreePresenter >> expandAll [

	self changed: #expandAll with: #()
]

{ #category : #api }
TreePresenter >> expandRoots [

	self changed: #expandRoots with: #()
]

{ #category : #'api-filtering' }
TreePresenter >> filterWith: aFilter [
	
	self selection: nil.
	self updateTree.

	self expandAll.
	self changed: #filterWith: with: {aFilter}.
	
	self expandAll.
]

{ #category : #'api-compatibility' }
TreePresenter >> hasChildrenBlock [
	"<api: #inspect>"
	"Get the block used to check if a node has children.
	The optional block arguments are:
		- the node
		- the tree"

	^ hasChildrenBlockHolder value
]

{ #category : #'api-compatibility' }
TreePresenter >> hasChildrenBlock: aBlock [
	"< api: #block getter: #hasChildrenBlock registration: #whenHasChildrenBlockChanged:>"
	"Set the block used to  check if a node has children.
	The optional block arguments are:
		- the node
		- the tree"

	hasChildrenBlockHolder value: aBlock
]

{ #category : #api }
TreePresenter >> hasChildrenFor: anItem [

	^ self hasChildrenBlock cull: anItem cull: self
]

{ #category : #api }
TreePresenter >> hasSelectedItem [
	"< api: #inspect>"
	"Return true if the tree contains any selected items"
	
	^ selectionHolder value isNotEmpty
]

{ #category : #api }
TreePresenter >> highlightedItem [
	"<api: #inspect>"
	"Return the currently highlighted item"

	^ highlightedItem value
]

{ #category : #api }
TreePresenter >> highlightedItem: anItem [
	"< api: #st getter: #highlightedItem registration: #whenHighlightedItemChanged:>"
	"Set the highlightedItem item"
	"In the case of keyboard navigation with checklist, it may differ from the last selected item"
	
	highlightedItem value: anItem
]

{ #category : #api }
TreePresenter >> iconBlock: aBlock [

	iconBlock value: aBlock
]

{ #category : #api }
TreePresenter >> iconFor: aNode [

	^ iconBlock value cull: aNode content cull: aNode
]

{ #category : #initialization }
TreePresenter >> initialize [
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
		TreeNodePresenter new 
			content: item;
			children: [ self childrenFor: item ] ] asValueHolder.
	
	columns := { TreeColumnPresenter new
		displayBlock: [:node | (self wrapItem: node content) ] } asValueHolder.
	
	autoRefreshOnExpand := false asValueHolder.
	
	"Since my nodes are already morphs, dragging them will actually extract them from their owner"
	dragTransformationBlock := [ :item :source | item complexContents ] asValueHolder.

	treeUpdated := nil asValueHolder.

	self registerEvents
]

{ #category : #api }
TreePresenter >> isCheckList [

	^ isCheckList value
]

{ #category : #api }
TreePresenter >> isCheckList: aBoolean [

	isCheckList value: aBoolean
]

{ #category : #api }
TreePresenter >> keyStroke: aKeyStroke [

	keyStroke value cull: aKeyStroke
]

{ #category : #api }
TreePresenter >> keyStrokeAction: aBlock [

	keyStroke value: aBlock
]

{ #category : #api }
TreePresenter >> menu [
	"<api: #inspect>"
	"Return the block used to generate the tree menu"

	^ menuBlockHolder value
]

{ #category : #api }
TreePresenter >> menu: aBlock [
"	< api: #block getter: #menu registration: #whenMenuChanged:>"
	"Set the block used to generate the tree menu"

	menuBlockHolder value: aBlock
]

{ #category : #api }
TreePresenter >> menu: aMenu shifted: aBoolean [
	| menu |
	
	(self allowMenuOnNoItem not and: [ self selectedItems isEmpty ])
		ifTrue: [ ^ nil ].
		
	
	menuBlockHolder value ifNil: [ ^ nil ].
	menuBlockHolder value isBlock ifFalse: [ ^ menuBlockHolder value ].
	
	menu := MenuPresenter new.
	
	menuBlockHolder value cull: menu cull: aBoolean.
	
	^ menu
]

{ #category : #api }
TreePresenter >> multiSelection [

	^ multiSelection value
]

{ #category : #api }
TreePresenter >> multiSelection: aBoolean [

	multiSelection value: aBoolean
]

{ #category : #api }
TreePresenter >> oddRowColor [

	^ oddRowColor value
]

{ #category : #api }
TreePresenter >> oddRowColor: aColor [

	oddRowColor value: aColor
]

{ #category : #initialization }
TreePresenter >> onWidgetBuilt [
	^ self selectedItems: selectionHolder value
]

{ #category : #api }
TreePresenter >> preferedPaneColor [

	^ preferedPaneColor value
]

{ #category : #api }
TreePresenter >> preferedPaneColor: aColor [

	preferedPaneColor value: aColor
]

{ #category : #initialization }
TreePresenter >> registerEvents [
	
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
					oldMenu := MenuPresenter new.
					oldMenuModel cull: oldMenu cull: false ]
				ifFalse: [ oldMenu := oldMenuModel ].
		oldMenu applyTo: self ].
		
		aMenuModel ifNotNil: [ | newMenu |
			aMenuModel isBlock
				ifTrue: [ 
					newMenu := MenuPresenter new.
					aMenuModel cull: newMenu cull: false ]
				ifFalse: [ newMenu := aMenuModel ].
		newMenu applyTo: self ] ].

	self announcer 
		when: WidgetBuilt
		send: #onWidgetBuilt
		to: self.
]

{ #category : #api }
TreePresenter >> removeOnlyLastSelected: aBoolean [

	removeOnlyLastSelected value: aBoolean
]

{ #category : #api }
TreePresenter >> resetSelection [

	self selectedItems: {}
]

{ #category : #api }
TreePresenter >> resizerWidth [

	^ resizerWidth value
]

{ #category : #api }
TreePresenter >> resizerWidth: anInteger [

	resizerWidth value: anInteger
]

{ #category : #api }
TreePresenter >> rootNodeFor: item [
	| node |
	
	node := self rootNodeHolder cull: item.
	node container: self.
	
	^ node
]

{ #category : #api }
TreePresenter >> rootNodeHolder [

	^ rootNodeHolder value
]

{ #category : #api }
TreePresenter >> rootNodeHolder: aBlock [

	rootNodeHolder value: aBlock
]

{ #category : #api }
TreePresenter >> roots [
	"< api: #inspect>"
	"Return the tree roots"

	^ rootsHolder value
]

{ #category : #api }
TreePresenter >> roots: aCollection [
	"< api: #st getter: #roots registration: #whenRootsChanged:>"
	"Set the tree roots"

	rootsHolder value: aCollection.
	self changed: #setRoots: with: { aCollection }
]

{ #category : #api }
TreePresenter >> rowInset [

	^ rowInset value
]

{ #category : #api }
TreePresenter >> rowInset: anInteger [

	rowInset value: anInteger
]

{ #category : #api }
TreePresenter >> secondSelection: anItem [
	
	self widget ifNotNil: [ :w | w listManager secondSelection: anItem ]
]

{ #category : #api }
TreePresenter >> selectAll [
	
	self changed: #selectAdd with: #()
]

{ #category : #api }
TreePresenter >> selectOnlyLastHighlighted [

	^ removeOnlyLastSelected value
]

{ #category : #api }
TreePresenter >> selectedItem [
	"< api: #inspect>"
	"Return the selected item"
	"In the case of a multiple selection list, it returns the last selected item"
	
	^ selectionHolder value
		ifEmpty: [ nil ]
		ifNotEmpty: [ :col | col  last ]
]

{ #category : #api }
TreePresenter >> selectedItem: anItem [
	"< api: #st getter: #selectedItem registration: #whenSelectedItemsChanged:>"
	"Set the selected item"
	"In the case of a multiple selection list, it returns the last selected item"
	
	self selectedItems: { anItem }
]

{ #category : #api }
TreePresenter >> selectedItems [
	"< api: #inspect>"
	"Return the selected items"
	
	^ selectionHolder value
]

{ #category : #api }
TreePresenter >> selectedItems: aCollection [
	"<api: #st getter: #selectedItems registration: #whenSelectedItemsChanged:>"
	"Set the selected item"
	"In the case of a multiple selection list, it returns the last selected item"

	"treePresenter selectNodePath: aCollection."
	selectionHolder value: aCollection.
	
]

{ #category : #api }
TreePresenter >> selection: aSelection [
 
	self changed: #selection: with: { aSelection }
]

{ #category : #api }
TreePresenter >> selectionChanged [
	
	self changed: #selectionChanged with: #()
]

{ #category : #initialization }
TreePresenter >> selectionChanged: ann [

	^ selectionHolder value: (ann selection
		ifNil: [ #() ]
		ifNotNil: [:selection | selection selectedNodes ])
]

{ #category : #api }
TreePresenter >> updateTree [
	"< api: #do>"
	"Force the tree to refresh"

	self changed: #updateTree with: {}.
	treeUpdated value: nil.
]

{ #category : #'api-events-compatibility' }
TreePresenter >> whenChildrenBlockChanged: aBlock [
	"Set a block to perform when a new childrenBlock is set"

	childrenBlockHolder whenChangedDo: aBlock
]

{ #category : #'api-events-compatibility' }
TreePresenter >> whenDisplayBlockChanged: aBlock [
	"Set a block to perform when a new displayBlock is set"

	displayBlock whenChangedDo: aBlock
]

{ #category : #'api-events-compatibility' }
TreePresenter >> whenHasChildrenBlockChanged: aBlock [
	"Set a block to perform when a new hasChildrenBlock is set"

	hasChildrenBlockHolder whenChangedDo: aBlock
]

{ #category : #'api-events' }
TreePresenter >> whenHighlightedItemChanged: aBlock [

	highlightedItem whenChangedDo: aBlock
]

{ #category : #'api-events' }
TreePresenter >> whenMenuChanged: aBlock [
	"Set a block to perform when a new menuBlock is set"

	menuBlockHolder whenChangedDo: aBlock
]

{ #category : #'api-events' }
TreePresenter >> whenRootsChanged: aBlock [

	rootsHolder whenChangedDo: aBlock
]

{ #category : #'api-events-compatibility' }
TreePresenter >> whenSelectedItemChanged: aBlock [
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

{ #category : #'api-events' }
TreePresenter >> whenSelectedItemsChanged: aBlock [
	"Set a block to perform when a new item is selected"

	selectionHolder whenChangedDo: [ :col | aBlock cull: (col collect: [:e | e content ]) ]
]

{ #category : #'api-events' }
TreePresenter >> whenTreeUpdated: aBlock [

	treeUpdated whenChangedDo: aBlock
]

{ #category : #api }
TreePresenter >> wrapItem: anItem [

	^ displayBlock value cull: anItem cull: self
]
