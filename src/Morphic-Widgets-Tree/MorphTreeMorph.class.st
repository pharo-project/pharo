"
A MorphTreeMorph is a list and a tree in one easily customizable widget. A list or tree is made of nodes. Each node can be made of whatever object . This allows the use of morphs inside the tree. A MorphTreeMorph works with a model which must use the TMorphTreeModel trait. MorphTreeModel uses it and can serves  as the model or as a superclass for a specific tree model.

Customizable columns:
Several customizable columns can be viewed. Columns are separated by resizers used in order to increase or decrease the columns width with the mouse.
A MorphTreeMorph can have a top header composed of buttons, one button per column. Such a button can have an icon and/or a title and may run some action when clicked on (a typical action is the ordering of the list). You can also allow column drag-and-drop so that a column  can be dynamically moved with a simple drop.
See this in action with following example:
-----------
ClassListExample new openOn: Collection
-----------
By default, the last column is not bounded, so that no resizer is added for it and threre exists no unused space between the last scroller and the right side of the whole tree. But, in some case one want to have a resizer also for the last column. This is the case for data grid as an example This is possible by sending #makeLastColumnBounded to the MorphTreeMorph.
Try it with:
-----------
SimpleGridExample new open
-----------

Single and multi-selection:
A MorphTreeMorph implements single and multiple selection. Multi-selection is allowed by sending  #multiSelection: with true as argument. Several items can be selected with ctrl-click (or cmd-click on mac) or with shift-click (see MorphTreeMorphModel comments to see how to handle selection from the model).
Try multi-selection with following example:
------------
SimplestClassListExample new openOn: Collection
------------

Double-click handling:
You can allow double-click just by indicating the message to send to the model with the doubleClickSelector: selector.
Try this with the package-tree example where double-clicking on a class node or or a method node open a browser on the class or on the method:
------------
PackageTreeExample new open
------------

Long list handling:
For very long lists or trees, two kind of pager can be used to limit the number of items visible in the list. The idea  is that when you have very long lists, you most of the time do not  want to see all details but just want some visual support for what is in the list: 
- with a simple pager, you indicate how much items are to be seen in one page, the list items are viewed page by page,
- with a chunk pager you can expand either incrementally or  all-together the number of items once you get to the bottom of the existing items.
See SimplestClassListWithPagerExample and SimplestClassListWithChunkExample examples.
Try them with:
------------
SimplestClassListWithPagerExample new openOn: Object.
SimplestClassListWithChunkExample new openOn: Object.
------------

Columns/rows coloring:
MorphTreeMorph makes it possible the coloring of either the columns or the rows. A MorphTreeMorph understands #rowColorForEven:odd: for rows coloring and columnColorForEven:odd: for columns coloring with two colors passed as argument (nil means no color). 
See following examples:
-------------
PackageTreeExample new open. ""For row coloring""
ClassListExample new openOn: Collection. ""For column coloring""
-------------

Column drag and drop
A column can be dragged. Inside the tree, a column can be dropped into another one. Then, the two columns are swapped (the roughly implemented)
Try it with:
-------------
ClassListExample new openOn: Collection.
-------------


Instance Variables
	autoDeselection:		<Object>
	autoMultiSelection:		<Object>
	columnColors:		<Object>
	columnDropUnabled:		<Object>
	columnInset:		<Object>
	columnResizers:		<Object>
	columns:		<Object>
	doubleClickSelector:		<Object>
	expandedToggleImage:		<Object>
	gapAfterIcon:		<Object>
	gapAfterToggle:		<Object>
	getListSelector:		<Object>
	getSelectionSelector:		<Object>
	hasToggleAtRoot:		<Object>
	iconReservedExtent:		<Object>
	indentGap:		<Object>
	keystrokeActionSelector:		<Object>
	lastSelectedMorph:		<Object>
	lineColor:		<Object>
	multipleSelection:		<Object>
	nodeList:		<Object>
	nodeSortBlock:		<Object>
	notExpandedToggleImage:		<Object>
	pager:		<Object>
	potentialDropMorph:		<Object>
	preferedPaneColor:		<Object>
	resizerWidth:		<Object>
	rowColors:		<Object>
	rowInset:		<Object>
	scrollDeltaHeight:		<Object>
	selectedMorphList:		<Object>
	setSelectionSelector:		<Object>
	shiftSelectedMorph:		<Object>
	topHeader:		<Object>
	topHeaderBackground:		<Object>
	unboundLastColumn:		<Object>
	withHLines:		<Object>

autoDeselection
	- xxxxx

autoMultiSelection
	- xxxxx

columnColors
	- xxxxx

columnDropUnabled
	- xxxxx

columnInset
	- xxxxx

columnResizers
	- xxxxx

columns
	- xxxxx

doubleClickSelector
	- xxxxx

expandedToggleImage
	- xxxxx

gapAfterIcon
	- xxxxx

gapAfterToggle
	- xxxxx

getListSelector
	- xxxxx

getSelectionSelector
	- xxxxx

hasToggleAtRoot
	- xxxxx

iconReservedExtent
	- xxxxx

indentGap
	- xxxxx

keystrokeActionSelector
	- xxxxx

lastSelectedMorph
	- xxxxx

lineColor
	- xxxxx

multipleSelection
	- xxxxx

nodeList
	- xxxxx

nodeSortBlock
	- xxxxx

notExpandedToggleImage
	- xxxxx

pager
	- xxxxx

potentialDropMorph
	- xxxxx

preferedPaneColor
	- xxxxx

resizerWidth
	- xxxxx

rowColors
	- xxxxx

rowInset
	- xxxxx

scrollDeltaHeight
	- xxxxx

selectedMorphList
	- xxxxx

setSelectionSelector
	- xxxxx

shiftSelectedMorph
	- xxxxx

topHeader
	- xxxxx

topHeaderBackground
	- xxxxx

unboundLastColumn
	- xxxxx

withHLines
	- xxxxx

"
Class {
	#name : #MorphTreeMorph,
	#superclass : #ScrollPane,
	#instVars : [
		'columns',
		'potentialDropMorph',
		'rowInset',
		'columnInset',
		'columnResizers',
		'withHLines',
		'preferedPaneColor',
		'indentGap',
		'expandedToggleImage',
		'notExpandedToggleImage',
		'resizerWidth',
		'gapAfterToggle',
		'hasToggleAtRoot',
		'topHeader',
		'topHeaderBackground',
		'unboundLastColumn',
		'columnDropUnabled',
		'columnColors',
		'rowColors',
		'nodeList',
		'iconBlock',
		'treeLineWidth',
		'lineColorBlock',
		'treeLineDashes',
		'listManager',
		'mouseOverAllowed',
		'maxNodeWidth',
		'enabled'
	],
	#category : #'Morphic-Widgets-Tree'
}

{ #category : #'instance creation' }
MorphTreeMorph class >> on: anObject [
	^ self new model: anObject

]

{ #category : #'dropping/grabbing' }
MorphTreeMorph >> acceptDroppingMorph: aMorph event: evt [
	"This message is sent when a morph is dropped onto a morph that has agreed 
	to accept the dropped morph by responding 'true' to the wantsDroppedMorph:Event: 
	message. This implementation relay the drop to the model then release the morph
	which was candidate fro a drop"
	self model
		acceptDroppingMorph: aMorph
		event: evt
		inMorph: self.
	self resetPotentialDropMorph.
	evt hand releaseMouseFocus: self.
	Cursor normal show.

]

{ #category : #'column handling' }
MorphTreeMorph >> addColumn: aTreeColumn [
	"add a column"
	self addColumn: aTreeColumn afterIndex: self columns size
]

{ #category : #'column handling' }
MorphTreeMorph >> addColumn: aTreeColumn afterIndex: aPosition [
	"add a column at a given index then update the list
	in order to take the new column into account"
	aTreeColumn container: self.
	self columns add: aTreeColumn afterIndex: aPosition.
	self columnsChanged.
	self updateColumnMorphs.
	self updateList


]

{ #category : #'column handling' }
MorphTreeMorph >> addColumnResizers [
	"add all needed column resizers"
	columnResizers := self columnResizerFrames
				withIndexCollect: [:frm :idx | 
					(MorphTreeResizerMorph container: self index: idx) bounds: (frm translateBy: (scroller offset x negated @ 0))].
	self addAllMorphs: columnResizers.
	self columnResizersToFront
]

{ #category : #'submorphs-add/remove' }
MorphTreeMorph >> addMorphsAfter: parentMorph fromCollection: aCollection [
	"Sent when expanding a node in order to add chilldren nodes after the expanded node"
	| priorMorph morphList  subs |
	priorMorph := nil.
	morphList := OrderedCollection new.
	
	"prepare the list of nodes to be added"
	aCollection do: [:item | 
		priorMorph := self indentingItemClass new 
			initWithContents: item 
			prior: priorMorph 
			forList: self
			indentLevel: parentMorph indentLevel + 1.
		morphList add: priorMorph.
		"Was this row expanded ? if true -> expand it
			again " 
			priorMorph isExpanded
				ifTrue: [priorMorph isExpanded: true.
					priorMorph
						addChildrenForList: self
						addingTo: morphList
						withExpandedItems: #()] ].
	
	"Set new child morphs index"
	1 to: morphList size do: [:i | | m | (m := morphList at: i) index: i + parentMorph index].
	"Add the new morph list in the scroller"
	scroller addAllMorphs: morphList after: parentMorph.
	"update next morphs index"
	subs := self allNodeMorphs.
	morphList last index to: subs size do: [:pos | (subs at: pos) index: pos].
	"set the new morphs widths according to columns width"
	self updateColumnMorphsWidth.
	morphList do: [ :e | e doLayoutIn: e layoutBounds ].
	^morphList
	

]

{ #category : #'submorphs-add/remove' }
MorphTreeMorph >> addMorphsTo: morphList from: aCollection withExpandedItems: expandedItems atLevel: newIndent [ 
	"Sent when building the list (by #buildContents), takes
	into accound old expanded items: they remain 
	expanded such that a list update don't change the list
	visual state"
	| priorMorph firstAddition |
	priorMorph := nil.
	firstAddition := nil.
	"also for the system progress bar"
	aCollection
		doWithIndex: [:item :idx | 
			priorMorph := self indentingItemClass new
						initWithContents: item
						prior: priorMorph
						forList: self
						indentLevel: newIndent.
			firstAddition
				ifNil: [firstAddition := priorMorph].
			morphList add: priorMorph.
			"Was this row expanded ? if true -> expand it
			again " 
			((item hasEquivalentIn: expandedItems)
					or: [priorMorph isExpanded])
				ifTrue: [priorMorph isExpanded: true.
					priorMorph
						addChildrenForList: self
						addingTo: morphList
						withExpandedItems: expandedItems]].
	^ firstAddition
]

{ #category : #'submorphs-add/remove' }
MorphTreeMorph >> addSubmorphsFromNodeList [
	self addSubmorphsFromNodeList: self currentNodelist previouslyExpanded: #() 

]

{ #category : #'submorphs-add/remove' }
MorphTreeMorph >> addSubmorphsFromNodeList: aNodeList previouslyExpanded: expandedNodeList [
	| morphList  |
	morphList := OrderedCollection new.
	self 
		addMorphsTo: morphList
		from: aNodeList
		withExpandedItems: expandedNodeList
		atLevel: 0.
	self insertNewMorphs: morphList.
	self listManager updateSelectionFromModel.
	self roots do: [:r | r updateChildrenRecursively].
	self updateColumnMorphs
	
]

{ #category : #updating }
MorphTreeMorph >> adjustSubmorphPositions [
	maxNodeWidth := 0.
	^ self adjustSubmorphPositionsOf: self allNodeMorphs startIdx: 1 startPos: 0@0 

]

{ #category : #updating }
MorphTreeMorph >> adjustSubmorphPositionsOf: aCollection startIdx: anIndex startPos: aStartPoint [
	| p idx |
	p := aStartPoint.
	idx := anIndex.
	aCollection do: [ :each | | h |
		h := each height.
		each index: idx.
		each bounds: (p extent: each width @ h).
		maxNodeWidth := maxNodeWidth max: (each fullBounds width).
		idx := idx + 1.
		p := p + (0@h)].
	
	self setScrollDeltas.
	^ p
	
]

{ #category : #'accessing colors' }
MorphTreeMorph >> adoptPaneColor: paneColor [
	"Pass on to the selection, the border"
	
	super adoptPaneColor: paneColor.
	paneColor ifNil: [^self].
	self color: (self preferedPaneColor ifNil: [ self theme backgroundColor ] ).

]

{ #category : #accessing }
MorphTreeMorph >> allNodeMorphs [
	"all list morphs"
	^ scroller submorphs
		
]

{ #category : #'dropping/grabbing' }
MorphTreeMorph >> allowColumnDrop [
	"allowing column drop means that a column 
	can be dropped into another one. The default behavior is to swap the
	two columns"
	columnDropUnabled := true
]

{ #category : #selection }
MorphTreeMorph >> autoDeselection: trueOrFalse [
	"Enable/disable autoDeselect (see class comment)"
	self listManager autoDeselection: trueOrFalse
]

{ #category : #selection }
MorphTreeMorph >> autoMultiSelection [
	^ self listManager autoMultiSelection
]

{ #category : #selection }
MorphTreeMorph >> autoMultiSelection: aBoolean [
	self listManager autoMultiSelection: aBoolean
]

{ #category : #selection }
MorphTreeMorph >> beCheckList [
	self isCheckList: true. 
]

{ #category : #selection }
MorphTreeMorph >> beMultiple [	
	self listManager multipleSelection: true
]

{ #category : #selection }
MorphTreeMorph >> beSingle [	
	self listManager multipleSelection: false
]

{ #category : #updating }
MorphTreeMorph >> buildContents [
	nodeList := nil.
	scroller removeAllMorphs.
	(self nodeList isNil or: [self nodeList isEmpty]) 
		ifTrue: [
			nodeList := nil.	
			^ self emptySelection].
	self addSubmorphsFromNodeList.

]

{ #category : #updating }
MorphTreeMorph >> buildTopHeader [
	| subs |
	subs := OrderedCollection new.
	self columns
		do: [:col |
			col container: self.
			subs add: (col header hResizing: #rigid; layoutChanged; yourself)].
	topHeader := Morph new fillStyle: self topHeaderBackground.
	self addMorph: topHeader.
	topHeader clipSubmorphs: true.
	topHeader borderColor: Color veryLightGray.
	topHeader color: Color transparent.
	topHeader borderWidth: 0.
	topHeader addAllMorphs: subs.
	topHeader bounds: (scroller left @ self top corner: scroller right @ (self top + self topHeaderHeight)).


]

{ #category : #'announce requests' }
MorphTreeMorph >> changeListRequest: anAnnounce [
	self updateList
]

{ #category : #'announce requests' }
MorphTreeMorph >> changeRequest: anAnnounce [
	^ anAnnounce change: self
]

{ #category : #'announce requests' }
MorphTreeMorph >> changeSelectionRequest: anAnnounce [
	self updateSelectionFromModel
]

{ #category : #enumeration }
MorphTreeMorph >> childrenDo: aBlock [
	self roots do: aBlock
]

{ #category : #'expanding-collapsing' }
MorphTreeMorph >> collapseAll [
	self updateContentsWithPreviouslyExpanded: Array new.

]

{ #category : #'expanding-collapsing' }
MorphTreeMorph >> collapseNodePath: aPath [
	self allNodeMorphs first collapseNodePath: aPath
]

{ #category : #'announce requests' }
MorphTreeMorph >> collapseRequest: anAnnounce [
	anAnnounce nodes
		ifEmpty: [^ self collapseAll]
		ifNotEmpty: [:nodes | self collapseNodePath: nodes]


]

{ #category : #'accessing colors' }
MorphTreeMorph >> columnColorForEven: evenColor [ 
	columnColors at: 2 put: nil.
	columnColors at: 1 put: evenColor.
	
]

{ #category : #'accessing colors' }
MorphTreeMorph >> columnColorForEven: evenColor odd: oddColor [
	columnColors at: 2 put: oddColor.
	columnColors at: 1 put: evenColor.
	
]

{ #category : #'accessing colors' }
MorphTreeMorph >> columnColorForOdd: oddColor [ 
	columnColors at: 1 put: nil.
	columnColors at: 2 put: oddColor.
	
]

{ #category : #'accessing colors' }
MorphTreeMorph >> columnColors [ 
	^ columnColors 
]

{ #category : #'column handling' }
MorphTreeMorph >> columnDropUnabled [
	"return true if column drop is enabled - see also #allowColumnDrop"
	^ columnDropUnabled ifNil: [columnDropUnabled := false]
]

{ #category : #geometry }
MorphTreeMorph >> columnInset [
	"Horizontal space between a resizer and a row morph"
	^ columnInset ifNil: [columnInset := 0]
]

{ #category : #geometry }
MorphTreeMorph >> columnInset: anInteger [
	"Change the horizontal space between a resizer and a row morph"
	columnInset := anInteger.

]

{ #category : #'column handling' }
MorphTreeMorph >> columnResizerFrames [
	"return all resizers bounds in a collection - used to update column resizers position"
	| xOffset frms |
	xOffset := 0.
	frms := self columns collect: [:col | 
		xOffset := self minResizerOffset max: (xOffset + col currentWidth).
		scroller left + xOffset @ self top corner: scroller left + xOffset + self resizerWidth @ scroller bottom].
	"If the last column is unbounded, then its frame is removed from the collection because 
	no resizer is added for the last column"
	self lastColumnUnbounded  
		ifTrue: [frms 
			ifNotEmpty: [frms removeLast]].
	^ frms collect: [:f | f translateBy: (scroller offset x negated @ 0)]
]

{ #category : #'column handling' }
MorphTreeMorph >> columnResizers [
	"return the resizers which make it possible 
	to resize columns horizontally with the mouse"
	^ columnResizers ifNil: [ columnResizers := OrderedCollection new ]
]

{ #category : #'column handling' }
MorphTreeMorph >> columnResizersToFront [
	"Column resizers should always be at top"
	self columnResizers do: [:cl | cl comeToFront; fillStyle: cl normalFillStyle].

]

{ #category : #'column handling' }
MorphTreeMorph >> columns [
	"Return a column definitions, if empty, return a collection with one column"
	^columns ifNil: [self columns: (OrderedCollection with: MorphTreeColumn new). columns]
]

{ #category : #'column handling' }
MorphTreeMorph >> columns: aListOfTreeColumn [
	"set the columns -  as a consequence, the topHeader is update (if present) and column resizers are added"

	self removeColumnResizers.
	topHeader ifNotNil: [ self removeMorph: topHeader ].
	columns := aListOfTreeColumn asOrderedCollection.
	aListOfTreeColumn do: [:col | col container: self].
	self buildTopHeader .
	self addColumnResizers.

]

{ #category : #'column handling' }
MorphTreeMorph >> columnsChanged [
	"A column has been added or removed or swapped with another one
	- rebuild all resizers and the top header"
	self removeColumnResizers.
	self removeTopHeader.
	self buildTopHeader.
	self addColumnResizers.

]

{ #category : #'events-processing' }
MorphTreeMorph >> commandOrCrontrolKeyPressed: anEvent [
	^ (Smalltalk os isMacOS) 
		ifTrue: [anEvent controlKeyPressed]
		ifFalse: [anEvent commandKeyPressed]
]

{ #category : #accessing }
MorphTreeMorph >> currentNodelist [
	"The nodeList currently viewed "
	^ self nodeList
]

{ #category : #'expanding-collapsing' }
MorphTreeMorph >> currentlyExpanded [

	^ self expandedNodesFrom: (self allNodeMorphs).
	
]

{ #category : #selection }
MorphTreeMorph >> deselectAll [
	self listManager deselectAll
]

{ #category : #accessing }
MorphTreeMorph >> disable [
	self enabled: false
]

{ #category : #'events-processing' }
MorphTreeMorph >> doubleClick: anEvent [ 
	| targetMorph |
	targetMorph := self scrollerSubMorphFromPoint: anEvent position.
	(self listManager doubleClick:  anEvent on: targetMorph)
		ifFalse: [super doubleClick: anEvent]

]

{ #category : #accessing }
MorphTreeMorph >> doubleClickSelector: aSelector [
	"set a  double click action"
	self listManager doubleClickSelector: aSelector
]

{ #category : #drawing }
MorphTreeMorph >> drawLinesOn: aCanvas [
	"Draw the lines for the submorphs.
	Modified for performance."
	self hasToggleAtRoot ifFalse: [^ self].
	aCanvas 
		transformBy: scroller transform
		clippingTo: scroller innerBounds
		during: [:clippedCanvas |
			scroller submorphsDo: [ :submorph | | last |
				((submorph isExpanded and: [
					(submorph nextSibling notNil and: [
						clippedCanvas isVisible: (submorph fullBounds topLeft
							corner: submorph nextSibling fullBounds bottomRight)]) or: [
					submorph nextSibling isNil and: [(last := submorph lastChild) notNil and: [
						clippedCanvas isVisible: (submorph fullBounds topLeft
							corner: last fullBounds bottomRight)]]]]) or: [
				(clippedCanvas isVisible: submorph fullBounds) or: [
				(submorph nextSibling notNil and: [
						clippedCanvas isVisible: submorph nextSibling fullBounds])]]) ifTrue:[
					submorph drawLinesOn: clippedCanvas]]]
		smoothing: scroller smoothing

]

{ #category : #drawing }
MorphTreeMorph >> drawOn: aCanvas [ 
	super drawOn: aCanvas.
	self columns do: [:col | col drawColumnOn: aCanvas].

	

]

{ #category : #selection }
MorphTreeMorph >> emptySelection [
	self listManager emptySelection
]

{ #category : #accessing }
MorphTreeMorph >> enable [
	self enabled: true
]

{ #category : #accessing }
MorphTreeMorph >> enabled [
	^ enabled ifNil: [ enabled := super enabled ]
]

{ #category : #accessing }
MorphTreeMorph >> enabled: aBoolean [
	enabled := aBoolean.
	self
		submorphsDo: [ :sm | 
			sm
				allMorphsDo: [ :m | 
					(m respondsTo: #enabled:)
						ifTrue: [ m enabled: aBoolean ] ] ].
	self changed: #enabled.
	self changed
]

{ #category : #updating }
MorphTreeMorph >> enabledFromModel [
	| val |
	self model ifNil: [ ^ self ].
	val := self model enabled.
	val ~= self enabled
		ifTrue: [ self enabled: val ] 
]

{ #category : #'expanding-collapsing' }
MorphTreeMorph >> expand: aMorph suchThat: aBlock [
	(aBlock value: aMorph complexContents)
		ifTrue: [
			aMorph isExpanded ifFalse: [aMorph expand].
			aMorph childrenDo: [:ch | self expandSilently: ch suchThat: aBlock]].
	self innerWidgetChanged.
]

{ #category : #'expanding-collapsing' }
MorphTreeMorph >> expand: aMorph to: level [
	| allChildren |
	aMorph toggleExpandedState.
	allChildren := OrderedCollection new: 10.
	aMorph recursiveAddTo: allChildren.
	allChildren do: [:each | 
		((each canExpand
			and: [each isExpanded not])
			and: [level > 0])
			ifTrue: [self expand: each to: level-1]].
]

{ #category : #'expanding-collapsing' }
MorphTreeMorph >> expandAll [
	"Expand all of the roots"
	
	self roots reverseDo: [:m | self expandAllSilently: m].
	self innerWidgetChanged
]

{ #category : #'expanding-collapsing' }
MorphTreeMorph >> expandAll: aMorph [
	|  subs |
	self expandAllSilently: aMorph.
	aMorph updateChildrenRecursively.
	subs := self scroller submorphs.
	1 to: subs size do: [:pos | (subs at: pos) index: pos].
	"set the new morphs widths according to columns width"
	self innerWidgetChanged.


]

{ #category : #'expanding-collapsing' }
MorphTreeMorph >> expandAll: aMorph except: aBlock [
	| allChildren |
	(aBlock value: aMorph complexContents)
		ifFalse: [^self].
	aMorph toggleExpandedState.
	allChildren := OrderedCollection new: 10.
	aMorph recursiveAddTo: allChildren.
	allChildren do: [:each | 
		(each canExpand
			and: [each isExpanded not])
			ifTrue: [self expandAll: each except: aBlock]].
]

{ #category : #'expanding-collapsing' }
MorphTreeMorph >> expandAllFromNode: aNode [
		self expandAll: ((self nodeMorphOfNode: aNode) ifNil: [^self]).
		self adjustSubmorphPositions

]

{ #category : #'expanding-collapsing' }
MorphTreeMorph >> expandAllSilently: aMorph [
	aMorph isExpanded ifFalse: [aMorph expand].
	aMorph childrenDo: [:ch | self expandAllSilently: ch].
	


]

{ #category : #'expanding-collapsing' }
MorphTreeMorph >> expandAllSuchThat: aBlock [
	self roots do: [:m | self expand: m suchThat: aBlock].
	self innerWidgetChanged
]

{ #category : #'expanding-collapsing' }
MorphTreeMorph >> expandAllTo: aLevel [
	self roots do: [:m | self expand: m to: aLevel].
	self innerWidgetChanged
]

{ #category : #'expanding-collapsing' }
MorphTreeMorph >> expandItemPath: aNodePath [
	(self allNodeMorphs at: 1 ifAbsent: [^self]) 
		openItemPath: aNodePath
]

{ #category : #'expanding-collapsing' }
MorphTreeMorph >> expandNodePath: aNodePath [
	(self allNodeMorphs at: 1 ifAbsent: [^self]) 
		openNodePath: aNodePath
]

{ #category : #'expanding-collapsing' }
MorphTreeMorph >> expandRoots [
	"Expand all the receiver's roots"
	
	self roots
		do: [:each |
			(each canExpand and: [each isExpanded not])
				ifTrue: [each toggleExpandedState]].
	self innerWidgetChanged
]

{ #category : #'expanding-collapsing' }
MorphTreeMorph >> expandSilently: aMorph suchThat: aBlock [
	(aBlock value: aMorph complexContents)
		ifTrue: [
			aMorph isExpanded ifFalse: [aMorph expand].
			aMorph childrenDo: [:ch | self expandSilently: ch suchThat: aBlock]].

]

{ #category : #'expanding-collapsing' }
MorphTreeMorph >> expandedForm [
	"Answer the form to use for expanded items."
	
	^self theme treeExpandedForm
]

{ #category : #'expanding-collapsing' }
MorphTreeMorph >> expandedFormForMorph: aMorph [
	"Answer the form to use for expanded items."
	
	^ (aMorph selected and: [self selectionColor luminance < 0.7])
		ifTrue: [self theme whiteTreeExpandedForm]
		ifFalse: [self theme treeExpandedForm]
]

{ #category : #'expanding-collapsing' }
MorphTreeMorph >> expandedNodesFrom: aMorpList [

	^ (aMorpList select: [ :each | each isExpanded]) collect: [ :each | each complexContents].
	
]

{ #category : #'expanding-collapsing' }
MorphTreeMorph >> expandedToggleImage [
	^ expandedToggleImage ifNil: [expandedToggleImage := ImageMorph new form: self expandedForm].

]

{ #category : #geometry }
MorphTreeMorph >> extent: newExtent [
	super extent: newExtent.
	self resizerChanged.

]

{ #category : #accessing }
MorphTreeMorph >> fillStyleToUse [
	"Answer the fillStyle that should be used for the receiver."
	
	^self enabled
		ifTrue: [self theme listNormalFillStyleFor: self]
		ifFalse: [self theme listDisabledFillStyleFor: self]
]

{ #category : #accessing }
MorphTreeMorph >> firstChild [
	"returns the first scroller submorph if not empty, or nil if empty"
	^ self firstNodeMorph
]

{ #category : #accessing }
MorphTreeMorph >> firstNodeMorph [
	"returns the first scroller submorph if not empty, or nil if empty"
	^ self allNodeMorphs ifEmpty: [nil] ifNotEmpty: [scroller submorphs at: 1]
]

{ #category : #'dropping/grabbing' }
MorphTreeMorph >> forbidColumnDrop [
	"Do not allow column drag and drop"
	columnDropUnabled := false
]

{ #category : #geometry }
MorphTreeMorph >> gapAfterToggle [
	"horizontal space after the toggle"
	^ gapAfterToggle ifNil: [gapAfterToggle := 5]
]

{ #category : #geometry }
MorphTreeMorph >> gapAfterToggle: anInteger [
	"set the horizontal space after the toggle"
	gapAfterToggle := anInteger.

]

{ #category : #accessing }
MorphTreeMorph >> getList [ 
	"Answer the full list to be displayed."
	^ model rootNodes.

]

{ #category : #scrolling }
MorphTreeMorph >> hScrollBarValue: scrollValue [
	| prev |
	prev := scroller offset x.
	super hScrollBarValue: scrollValue.
	scroller offset x ~= prev
		ifTrue: [
			self updateColumnResizersXOffset.
			self updateTopHeader]
]

{ #category : #'events-processing' }
MorphTreeMorph >> handleMouseMove: anEvent [
	"Reimplemented because we really want #mouseMove when a morph is dragged around"
	
	anEvent wasHandled 
		ifTrue:[^self]. 	
	(anEvent isDraggingEvent or:[ anEvent anyButtonPressed and:[anEvent hand mouseFocus == self]]) 
		ifFalse:[^self].
	anEvent wasHandled: true.
	self mouseMove: anEvent.
	(self handlesMouseStillDown: anEvent) 
		ifTrue: [
			"Step at the new location"
			self startStepping: #handleMouseStillDown: 
				at: Time millisecondClockValue + self mouseStillDownThreshold
				arguments: {anEvent copy resetHandlerFields}
				stepTime: self mouseStillDownStepRate].

]

{ #category : #'event handling' }
MorphTreeMorph >> handlesKeyboard: evt [
	^true
]

{ #category : #'event handling' }
MorphTreeMorph >> handlesMouseOver: evt [
	^ self mouseOverAllowed 
]

{ #category : #'event handling' }
MorphTreeMorph >> handlesMouseOverDragging: evt [
	^self dropEnabled
]

{ #category : #'event handling' }
MorphTreeMorph >> handlesMouseStillDown: anEvent [
	"Still down event is used to scroll the selection 
	when the mouse is outside (upon the top of 
	below the bottom)"
	^ (self innerBounds containsPoint: anEvent position) not
]

{ #category : #accessing }
MorphTreeMorph >> hasIconBlock [
	^ iconBlock notNil
]

{ #category : #accessing }
MorphTreeMorph >> hasToggleAtRoot [
	^ hasToggleAtRoot ifNil: [hasToggleAtRoot := self roots anySatisfy: [:s | s hasToggle]]

]

{ #category : #'column handling' }
MorphTreeMorph >> headerBounds [
	"return the bounds of each top header button (one per column)"

	| positions controlBounds currPos currLeft |
	controlBounds := OrderedCollection new.
	currPos := scroller left.
	(positions := self columnResizers asOrderedCollection collect: [ :r | r position ])
		ifNotEmpty: [ 
			| currRight |
			currPos := positions removeFirst x.
			currLeft := scroller left - scroller offset x.
			currRight := currPos.
			controlBounds add: (currLeft @ topHeader top corner: currRight @ topHeader bottom).
			[ positions notEmpty ]
				whileTrue: [ 
					currLeft := currPos + self resizerWidth.
					currPos := positions removeFirst x.
					currRight := currPos.
					controlBounds add: (currLeft @ topHeader top corner: currRight @ topHeader bottom) ] ].
	self columnResizers size < self columns size
		ifTrue: [ 
			currLeft := currPos + self resizerWidth.
			controlBounds add: ((currLeft min: scroller right) @ topHeader top corner: scroller right @ topHeader bottom) ].
	^ controlBounds
]

{ #category : #accessing }
MorphTreeMorph >> iconBlock [
	^ iconBlock ifNil: [[:node | node icon]]
]

{ #category : #accessing }
MorphTreeMorph >> iconBlock: aValuableWithOneArg [
	"A valuable which value is an icon or nil. takes a node as argument"
	iconBlock := aValuableWithOneArg
]

{ #category : #geometry }
MorphTreeMorph >> indentGap [
	^ indentGap ifNil: [indentGap := 20]
]

{ #category : #geometry }
MorphTreeMorph >> indentGap: anInteger [
	indentGap := anInteger.

]

{ #category : #accessing }
MorphTreeMorph >> indentingItemClass [
	
	^ MorphTreeNodeMorph
]

{ #category : #initialization }
MorphTreeMorph >> initialize [
	"initialize the state of the receiver"
	super initialize.
	columnColors := Array 
		with: Color transparent 
		with: Color transparent.
	rowColors := Array 
		with: self theme backgroundColor 
		with: self theme backgroundColor.
	self borderWidth: 0.

	

]

{ #category : #keymapping }
MorphTreeMorph >> initializeShortcuts: aKMDispatcher [

	super initializeShortcuts: aKMDispatcher.
	aKMDispatcher attachCategory: #MorphFocusNavigation
]

{ #category : #geometry }
MorphTreeMorph >> innerBounds [
	| inner |
	inner := super innerBounds.
	inner := inner withTop: self top + self topHeaderHeight.
	^ inner
]

{ #category : #updating }
MorphTreeMorph >> innerWidgetChanged [
	self setScrollDeltas.
	self updateColumnMorphsWidth.
	self adjustSubmorphPositions.

]

{ #category : #updating }
MorphTreeMorph >> insertNewMorphs: morphList [
	scroller addAllMorphs: morphList.

]

{ #category : #selection }
MorphTreeMorph >> isCheckList: aBoolean [
	self listManager isCheckList: aBoolean. 
]

{ #category : #selection }
MorphTreeMorph >> isSingle [	
	^ self isMultiple not
]

{ #category : #accessing }
MorphTreeMorph >> itemStringGetter: aValuable [
	"Set how to get a string for the first column with a valuable which 
	takes a row item (from a MorphTreeMorphNode point of view, its complexContents item) as argument"
	self columns first itemStringGetter: aValuable
]

{ #category : #'events-processing' }
MorphTreeMorph >> keyStroke: event [ 
	"Process potential command keys."
	(self navigationKey: event) ifTrue: [^true].
	(self scrollByKeyboard: event) ifTrue: [^true].
	^ self listManager keyStroke: event
]

{ #category : #'event handling' }
MorphTreeMorph >> keyboardFocusChange: aBoolean [
	"The message is sent to a morph when its keyboard focus changes.
	Update for focus feedback."
	super keyboardFocusChange: aBoolean.
	self focusChanged
]

{ #category : #accessing }
MorphTreeMorph >> keystrokeActionSelector: aSelector [
	self listManager keystrokeActionSelector: aSelector
]

{ #category : #'column handling' }
MorphTreeMorph >> lastColumnBounded [
	"Return true if the last column can be resized with a resizer"
	^ self lastColumnUnbounded not
]

{ #category : #'column handling' }
MorphTreeMorph >> lastColumnUnbounded [
	"Return true if the last column can not be resized with a resizer"
	^ unboundLastColumn ifNil: [unboundLastColumn := true]
]

{ #category : #'accessing colors' }
MorphTreeMorph >> lineColor: aColor [
	lineColorBlock := [:node | aColor]

]

{ #category : #'accessing colors' }
MorphTreeMorph >> lineColorBlock [
	^ lineColorBlock

]

{ #category : #'accessing colors' }
MorphTreeMorph >> lineColorBlock: aValuable [
	lineColorBlock := aValuable

]

{ #category : #'accessing colors' }
MorphTreeMorph >> lineColorForNode: aNode [
	"Answer a good color to use for drawing the lines that connect members of the hierarchy view.
	Used the cached color, or derive it if necessary by finding the receiver or 
	the first owner (up to my root) that is not transparent, then picking a contrasting color.
	Fall back to black if all my owners are transparent."

	^ lineColorBlock
		ifNotNil: [lineColorBlock value: aNode]
		ifNil: [
			| colored |
			colored := self color isTransparent
				ifTrue: [self firstOwnerSuchThat: [:o | o isWorldOrHandMorph not and: [o color isTransparent not]]]
				ifFalse: [self].
			colored 
				ifNil: [Color black]
				ifNotNil: [colored color luminance > 0.5
					ifTrue: [Color black]
					ifFalse: [Color white]]]
	

]

{ #category : #accessing }
MorphTreeMorph >> listManager [
	^ listManager ifNil: [listManager := MorphTreeListManager new client: self] 
]

{ #category : #'column handling' }
MorphTreeMorph >> makeLastColumnBounded [
	"Make the last column horizontally resizable with a resizer"
	self lastColumnBounded 
		ifFalse: [
			unboundLastColumn := false.
			self columnsChanged]

]

{ #category : #'column handling' }
MorphTreeMorph >> makeLastColumnUnbounded [
	"Make the last column not horizontally  resizable (no resizer for it)"
	unboundLastColumn := true

]

{ #category : #geometry }
MorphTreeMorph >> maxNodeWidth [
	^ maxNodeWidth ifNil: [maxNodeWidth := 0]
]

{ #category : #geometry }
MorphTreeMorph >> minResizerOffset [
	^ 20
]

{ #category : #geometry }
MorphTreeMorph >> minResizerX [
	^ scroller left + self minResizerOffset
]

{ #category : #accessing }
MorphTreeMorph >> model: aTreeModel [
	self model ifNotNil: [self model announcer unsubscribe: self].
	super model: aTreeModel.
	self registerRequestHandlers

]

{ #category : #'events-processing' }
MorphTreeMorph >> mouseDown: event [ 
	"Changed to take keybaord focus."
	| targetMorph selectors |
	self enabled ifFalse: [ ^self ].
	mouseOverAllowed := true.
	self wantsKeyboardFocus
		ifTrue: [self takeKeyboardFocus].
	(event yellowButtonPressed and: [(self commandOrCrontrolKeyPressed: event) not])
		ifTrue: ["First check for option (menu) click"			
			^ self yellowButtonEvent: event].
	(targetMorph := self scrollerSubMorphFromPoint: event position)
		ifNotNil: [targetMorph hasToggle
				ifTrue: [(targetMorph
							inToggleArea: (targetMorph point: event position from: self))
						ifTrue: [^ self toggleExpandedState: targetMorph event: event]]].
	targetMorph
		ifNil: [^ super mouseDown: event].
	targetMorph highlightForMouseDown.
	selectors := Array
				with: #click:
				with: #doubleClick:
				with: nil
				with: (self dragEnabled
						ifTrue: [#startDrag:]).
	event hand
		waitForClicksOrDrag: self
		event: event
		selectors: selectors
		threshold: 10.

	self listManager mouseDown: event on: targetMorph


]

{ #category : #accessing }
MorphTreeMorph >> mouseDownHighlightColor [
	"Answer a good color to use for drawing the mouse down highlight.
	Used the line color if not transparent, otherwise a contrasting color in the
	same way as the line color is determined.
	Fall back to black if all my owners are transparent."

	|colored |
	colored := self color isTransparent
		ifTrue: [self firstOwnerSuchThat: [:o | o isWorldOrHandMorph not and: [o color isTransparent not]]]
		ifFalse: [self].
	colored ifNil: [^Color black].
	^colored color luminance > 0.5
		ifTrue: [Color black]
		ifFalse: [Color white]

]

{ #category : #'events-processing' }
MorphTreeMorph >> mouseEnter: event [
	"Changed to take keyboardFocusOnMouseDown preference into account."
	
	super mouseEnter: event.
	self wantsKeyboardFocus ifFalse: [^self].
	self keyboardFocusOnMouseDown ifFalse: [self takeKeyboardFocus]
]

{ #category : #'events-processing' }
MorphTreeMorph >> mouseEnterDragging: evt [
	| aMorph |
	(evt hand hasSubmorphs and: [self dropEnabled]) 
		ifFalse: [^super mouseEnterDragging: evt].
	(self wantsDroppedMorph: evt hand firstSubmorph event: evt)
		ifTrue: [
			aMorph := self scrollerSubMorphFromPoint: evt position.
			aMorph ifNotNil:[self potentialDropMorph: aMorph].
			evt hand newMouseFocus: self.
			"above is ugly but necessary for now"].
]

{ #category : #'events-processing' }
MorphTreeMorph >> mouseLeaveDragging: anEvent [
	(self dropEnabled and:[anEvent hand hasSubmorphs]) ifFalse: ["no d&d"
		^ super mouseLeaveDragging: anEvent].
	self resetPotentialDropMorph.
	anEvent hand releaseMouseFocus: self.
	"above is ugly but necessary for now"

]

{ #category : #'events-processing' }
MorphTreeMorph >> mouseMove: evt [

	| targetMorph |
	self enabled ifFalse: [ ^self ].
	targetMorph := self scrollerSubMorphFromPoint: evt position.
	evt hand hasSubmorphs 
		ifFalse: [(self innerBounds containsPoint: evt position)
			ifTrue: [self listManager mouseMove: evt on: targetMorph]].
		
	(self dropEnabled and: [evt hand hasSubmorphs]) 
		ifFalse:[^super mouseMove: evt].
		
	potentialDropMorph 
		ifNotNil:[
			(potentialDropMorph containsPoint: (potentialDropMorph point: evt position from: self))
				ifTrue:[^self]].
			
	self mouseLeaveDragging: evt.
	
	(self containsPoint: evt position) 
		ifTrue: [self mouseEnterDragging: evt].
]

{ #category : #'event handling' }
MorphTreeMorph >> mouseOverAllowed [
	^ mouseOverAllowed ifNil: [mouseOverAllowed := false]
]

{ #category : #'events-processing' }
MorphTreeMorph >> mouseStillDown: anEvent [
	(anEvent hand position y > self innerBounds bottom)
		ifTrue: [self listManager selectMoreAtBottom]
		ifFalse: [
			(anEvent hand position y < self innerBounds top)
				ifTrue: [self listManager selectMoreAtTop]
				ifFalse: [super mouseStillDown: anEvent]]
]

{ #category : #'events-processing' }
MorphTreeMorph >> mouseStillDownStepRate [
	"At what rate do I want to receive #mouseStillDown: notifications?"
	^10
]

{ #category : #'events-processing' }
MorphTreeMorph >> mouseUp: event [ 
	"Fixed up highlight problems."
	| nodeMorph wasHigh |
	
	self enabled ifFalse: [ ^self ].
	mouseOverAllowed := false.
	nodeMorph := self scrollerSubMorphFromPoint: event position.

	wasHigh := nodeMorph notNil 
		ifTrue: [nodeMorph highlightedForMouseDown] 
		ifFalse: [false].
		
	self allNodeMorphs 
		do: [:m | 
				m highlightedForMouseDown 
					ifTrue: [m highlightForMouseDown: false]].
	wasHigh 
		ifFalse: [
			nodeMorph 
				ifNotNil: [^self]].

	self listManager mouseUp: event on: nodeMorph.

	
]

{ #category : #selection }
MorphTreeMorph >> multiSelection: aBoolean [
	self listManager multipleSelection: aBoolean
]

{ #category : #scrolling }
MorphTreeMorph >> newTransformMorph [
	^ MorphTreeTransformMorph new 
]

{ #category : #'announce requests' }
MorphTreeMorph >> nodeCollapseRequest: anAnnounce [
	anAnnounce nodes
		ifEmpty: [self collapseAll]
		ifNotEmpty: [:nodes | self collapseNodePath: nodes]

]

{ #category : #'announce requests' }
MorphTreeMorph >> nodeExpandRequest: anAnnounce [
	anAnnounce nodes
		ifEmpty: [self expandAll]
		ifNotEmpty: [:nodes | anAnnounce recur 
			ifTrue: [self expandAllFromNode: nodes last]
			ifFalse: [self expandNodePath: nodes]]

]

{ #category : #accessing }
MorphTreeMorph >> nodeList [
	^ nodeList ifNil: [nodeList := self getList]

]

{ #category : #accessing }
MorphTreeMorph >> nodeList: aCollection [ 
	nodeList := aCollection.

]

{ #category : #accessing }
MorphTreeMorph >> nodeListSelector [
	^ #rootNodes

]

{ #category : #accessing }
MorphTreeMorph >> nodeMorphOfNode: aNode [
	^ self allNodeMorphs detect: [:m | m complexContents  = aNode] ifNone: []
]

{ #category : #accessing }
MorphTreeMorph >> nodeStringGetter: aValuable [
	"Set how to get a string for the first column node with a valuable which 
	takes a row  MorphTreeMorphNode as argument"
	self columns first nodeStringGetter: aValuable
]

{ #category : #'expanding-collapsing' }
MorphTreeMorph >> notExpandedForm [
	"Answer the form to use for unexpanded items."
	
	^self theme treeUnexpandedForm
]

{ #category : #'expanding-collapsing' }
MorphTreeMorph >> notExpandedFormForMorph: aMorph [
	^ (aMorph selected and: [self selectionColor luminance < 0.7])
		ifTrue: [self theme whiteTreeUnexpandedForm]
		ifFalse: [self theme treeUnexpandedForm]

]

{ #category : #'expanding-collapsing' }
MorphTreeMorph >> notExpandedToggleImage [
	^ notExpandedToggleImage ifNil: [notExpandedToggleImage := ImageMorph new form: self notExpandedForm].

]

{ #category : #updating }
MorphTreeMorph >> noteRemovalOfAll: aCollection [
	"TODO: update the selection as well"

	 self listManager noteRemovalOfAll: aCollection.
	aCollection do: [ :each | each delete ].
	scroller removeAllMorphsIn: aCollection.
	self adjustSubmorphPositions
]

{ #category : #'dropping/grabbing' }
MorphTreeMorph >> potentialDropMorph [
	"return the morph (the MorphTreeNodeMorph) 
	which is the current drop target candidate "
	^potentialDropMorph
]

{ #category : #'dropping/grabbing' }
MorphTreeMorph >> potentialDropMorph: aMorph [
	"Set the morph (the MorphTreeNodeMorph) 
	which is the current drop target candidate "
	potentialDropMorph := aMorph.
	aMorph highlightForDrop
]

{ #category : #'accessing colors' }
MorphTreeMorph >> preferedPaneColor [
	^ preferedPaneColor
]

{ #category : #'accessing colors' }
MorphTreeMorph >> preferedPaneColor: aColor [
	self color: (preferedPaneColor := aColor).

]

{ #category : #updating }
MorphTreeMorph >> privateUpdateColumnMorphs [
	self resetRootInfo.
	self updateTopHeader.
	self innerWidgetChanged.

]

{ #category : #'announce requests' }
MorphTreeMorph >> registerRequestHandlers [
	self model announcer when: MorphTreeChangeRequest send: #changeRequest: to: self.
]

{ #category : #'initialize - release' }
MorphTreeMorph >> release [
	lineColorBlock := nil.
	columnResizers := nil.
	preferedPaneColor := nil.
	expandedToggleImage := nil.
	notExpandedToggleImage := nil.
	columns 
		ifNotNil: [
			columns do: [:col | col release]. 
			columns := nil].
	listManager 
		ifNotNil: [
			listManager release.
			listManager := nil].
	self releaseActionMap. "we are not sure if we need it"
	super release.
]

{ #category : #'column handling' }
MorphTreeMorph >> removeColumn: aTreeColumn [
	"Remove a column - rough implementation"
	self removeColumnAtIndex: (self columns indexOf: aTreeColumn)
]

{ #category : #'column handling' }
MorphTreeMorph >> removeColumnAtIndex: aPosition [
	"remove a column at a given position - rough implementation"
	self columns removeAt: aPosition.
	self columnsChanged.
	self updateList.





]

{ #category : #'column handling' }
MorphTreeMorph >> removeColumnResizers [
	"Remove all column resizers"
	self removeAllMorphsIn: self columnResizers.
	self columnResizers do: [:r | r release].
	self columnResizers removeAll
]

{ #category : #selection }
MorphTreeMorph >> removeOnlyLastSelected: aBoolean [
	self listManager removeOnlyLastSelected: aBoolean
]

{ #category : #updating }
MorphTreeMorph >> removeTopHeader [
	topHeader 
		ifNotNil: [self removeMorph: topHeader.
		topHeader := nil]

]

{ #category : #'dropping/grabbing' }
MorphTreeMorph >> resetPotentialDropMorph [
	"release the current drop morph candidate"
	potentialDropMorph ifNotNil: [
		potentialDropMorph resetHighlightForDrop.
		potentialDropMorph := nil]

]

{ #category : #updating }
MorphTreeMorph >> resetRootInfo [
	hasToggleAtRoot := nil.
]

{ #category : #updating }
MorphTreeMorph >> resizerChanged [
	self columns size > 1
		ifTrue: [
			self updateColumnResizersXOffset;
				resizeScroller;
				updateColumnMorphs]
		ifFalse: [self updateTopHeader]
]

{ #category : #geometry }
MorphTreeMorph >> resizerWidth [
	^ resizerWidth ifNil:[resizerWidth := 3]
]

{ #category : #geometry }
MorphTreeMorph >> resizerWidth: anInteger [
	resizerWidth := anInteger.

]

{ #category : #accessing }
MorphTreeMorph >> roots [
	"Answer the receiver's roots"
	^ self rootsFrom: self allNodeMorphs

]

{ #category : #accessing }
MorphTreeMorph >> rootsFrom: aCollectionOfNodes [
	"Answer the receiver's roots"
	^ aCollectionOfNodes
		select: [:each | each indentLevel isZero]
]

{ #category : #'accessing colors' }
MorphTreeMorph >> rowColorForEven: evenColor [ 
	rowColors at: 1 put: evenColor.
	
]

{ #category : #'accessing colors' }
MorphTreeMorph >> rowColorForEven: evenColor odd: oddColor [
	rowColors at: 2 put: oddColor.
	rowColors at: 1 put: evenColor.
	
]

{ #category : #'accessing colors' }
MorphTreeMorph >> rowColorForOdd: oddColor [ 
	rowColors at: 2 put: oddColor.
	
]

{ #category : #'accessing colors' }
MorphTreeMorph >> rowColors [
	^ rowColors
]

{ #category : #geometry }
MorphTreeMorph >> rowInset [
	^ rowInset  ifNil: [rowInset := 0]

]

{ #category : #geometry }
MorphTreeMorph >> rowInset: anInteger [
	rowInset := anInteger.

]

{ #category : #'column handling' }
MorphTreeMorph >> rowMorphsWidths [
	"Return all row morphs witdhs based on the header bounds. 
	used when a resizer is moved horizontally or if some 
	change implies that the list is rebuilt"
	| result |
	result :=  self headerBounds collect: [:b | b width].
	result ifNotEmpty: [result at: 1 put: ((result at: 1) - 3 )].
	^ result

]

{ #category : #scrolling }
MorphTreeMorph >> scrollDeltaWidth [
	"A guess -- assume that the width of a char is approx 1/2 the height of the font"
	^ self scrollDeltaHeight // 2



]

{ #category : #scrolling }
MorphTreeMorph >> scrollSelectionIntoView [
	"make sure that the current selection is visible"
	self listManager selectedMorph
		ifNotNil: [:morph | self scrollToShow: morph contentBounds ]
]

{ #category : #'events-processing' }
MorphTreeMorph >> scrollerSubMorphFromPoint: aPoint [
	"Return the list element (morph) at the given point or nil if outside"

	| ptY |
	scroller hasSubmorphs ifFalse: [ ^ nil ].
	ptY := (scroller firstSubmorph point: aPoint from: self) y.	
	"note: following assumes that submorphs are vertical, non-overlapping, and ordered"
	scroller firstSubmorph top > ptY ifTrue: [ ^ nil ].
	scroller lastSubmorph bottom < ptY ifTrue: [ ^ nil ].
	"now use binary search"
	^ scroller
		findSubmorphBinary: [ :item | 
			(ptY between: item top and: item bottom)
				ifTrue: [ 0 ]
				ifFalse: [ 
					(item top + item bottom) // 2 > ptY
						ifTrue: [ -1 ]
						ifFalse: [ 1 ] ]	"found" ]
]

{ #category : #selection }
MorphTreeMorph >> secondarySelectionColor [

	^ self theme secondarySelectionColor
]

{ #category : #selection }
MorphTreeMorph >> selectAll [
	self listManager selectAll
]

{ #category : #'announce requests' }
MorphTreeMorph >> selectItems: aNodeItemCollection [
	self selectedItems: aNodeItemCollection

]

{ #category : #'announce requests' }
MorphTreeMorph >> selectItemsRequest: anAnnounce [
	self selectedItems: anAnnounce itemsToSelect 
]

{ #category : #selection }
MorphTreeMorph >> selectNodePath: aPath [
	aPath 
		ifNil: [self emptySelection] 
		ifNotNil: [self firstChild 
			ifNotNil: [:fc | fc selectNodePath: aPath]]
]

{ #category : #selection }
MorphTreeMorph >> selectedItems: aNodeItemCollection [
	self listManager selectedItems: aNodeItemCollection
]

{ #category : #selection }
MorphTreeMorph >> selectedMorph [
	^ self listManager selectedMorph
]

{ #category : #selection }
MorphTreeMorph >> selectedMorphList [
	^ self listManager selectedMorphList
]

{ #category : #selection }
MorphTreeMorph >> selectionChanged [
	self changed

]

{ #category : #selection }
MorphTreeMorph >> selectionColor [
	"Answer the colour to use for selected items."

	^ self 
		valueOfProperty: #selectionColor 
		ifAbsent: [ self theme selectionColor ]
]

{ #category : #selection }
MorphTreeMorph >> selectionColor: aColor [
	"Set the colour for selected items."

	| window |
	aColor
		ifNil: [ self removeProperty: #selectionColor ]
		ifNotNil: [ self setProperty: #selectionColor toValue: aColor ].

	window := self ownerThatIsA: SystemWindow.

	self selectionColorToUse: ((self theme fadedBackgroundWindows not or: [ window isNil or: [ window isActive ] ])
		ifTrue: [ aColor ]
		ifFalse: [ self theme unfocusedSelectionColor ])
]

{ #category : #selection }
MorphTreeMorph >> selectionColorToUse [
	"Answer the colour to use for selected items."
	self enabled ifFalse: [ ^ self paneColor ].
	^ self valueOfProperty: #selectionColorToUse 
		ifAbsent: [ self theme selectionColor ].
]

{ #category : #selection }
MorphTreeMorph >> selectionColorToUse: aColor [
	"Set the colour for selected items."

	aColor = self selectionColorToUse ifTrue: [^self].
	aColor
		ifNil: [self removeProperty: #selectionColorToUse]
		ifNotNil: [self setProperty: #selectionColorToUse toValue: aColor].

]

{ #category : #selection }
MorphTreeMorph >> selectionFrameFor: aNodeMorph [
	"Answer the frame of aNodeMorph in the receiver"
	^ aNodeMorph bounds:  aNodeMorph selectionFrame in: self
]

{ #category : #accessing }
MorphTreeMorph >> setSelectedMorph: aNodeMorph [ 
	self listManager setSelectedMorph: aNodeMorph
]

{ #category : #'events-processing' }
MorphTreeMorph >> startDrag: anEvent [ 
	| aTransferMorph itemMorph passenger |
	self dragEnabled
		ifTrue: [itemMorph := self allNodeMorphs
						detect: [:any | any highlightedForMouseDown]
						ifNone: []].
	(itemMorph isNil
			or: [anEvent hand hasSubmorphs])
		ifTrue: [^ self].
	itemMorph highlightForMouseDown: false.
	
	(self listManager selectedMorphList includes: itemMorph)
		ifFalse: [self listManager setSelectedMorph: itemMorph].
		
	passenger := self model dragPassengerFor: itemMorph inMorph: self.
	passenger
		ifNotNil: [
			aTransferMorph := self model transferFor: passenger from: self.
				"Ask the draggedMorph otherwise the transferMorph has not yet its bounds"
			aTransferMorph align: aTransferMorph draggedMorph center with: anEvent position.
			aTransferMorph
				dragTransferType: (self model dragTransferTypeForMorph: self).
			anEvent hand grabMorph: aTransferMorph].
	anEvent hand releaseMouseFocus: self
]

{ #category : #'column handling' }
MorphTreeMorph >> swapColumn: aColumn withColumn: anotherColumn [
	"column swapping - this is the default behavior for column drag & drop"
	self swapColumnAt:  aColumn index withColumnAt: anotherColumn index
]

{ #category : #'column handling' }
MorphTreeMorph >> swapColumnAt: oneIndex withColumnAt: anotherIndex [
	"swap two column - very rough implementation which 
	simply rebuild everything"
	self columns swap: oneIndex with: anotherIndex. 
	self columnsChanged.
	self updateList.
	self columnResizersToFront

]

{ #category : #'events-processing' }
MorphTreeMorph >> takesKeyboardFocus [
	"Answer whether the receiver can normally take keyboard focus."

	^ self enabled
]

{ #category : #updating }
MorphTreeMorph >> themeChanged [
	
	rowColors at: 1 put: (	
		rowColors asSet size = 1 
			ifTrue: [  self theme backgroundColor ]
			ifFalse: [ rowColors at: 1 put: self theme backgroundColor contrastingColorAdjustment ]
		).
		
	rowColors at: 2 put: self theme backgroundColor .
	
	super themeChanged.
]

{ #category : #'events-processing' }
MorphTreeMorph >> toggleExpandedState: aMorph event: event [
	| oldState |
	event yellowButtonPressed 
		ifTrue: [
			oldState := aMorph isExpanded.
			self allNodeMorphs copy do: [ :each |
				(each canExpand and: [each isExpanded = oldState]) 
					ifTrue: [each toggleExpandedState]]] 
		ifFalse: [aMorph toggleExpandedState].
	self adjustSubmorphPositions.
	
]

{ #category : #'expanding-collapsing' }
MorphTreeMorph >> toggleImageHeight [
	^ self expandedToggleImage height max: self notExpandedToggleImage height.
]

{ #category : #'expanding-collapsing' }
MorphTreeMorph >> toggleImageWidth [
	^ self expandedToggleImage width max: self notExpandedToggleImage width.
]

{ #category : #accessing }
MorphTreeMorph >> topHeader [
	^ topHeader 
]

{ #category : #accessing }
MorphTreeMorph >> topHeaderBackground [
	^ topHeaderBackground ifNil: [topHeaderBackground := Color transparent]
]

{ #category : #accessing }
MorphTreeMorph >> topHeaderBackground: aFillStyle [
	topHeaderBackground := aFillStyle
]

{ #category : #accessing }
MorphTreeMorph >> topHeaderHeight [
	| h |
	self topHeader 
		ifNil: [^ 0] 
		ifNotNil: [:th | 
			h := th borderWidth * 2.
			self columns do: [:col | h := h max: col height].
			^ h]
		
]

{ #category : #accessing }
MorphTreeMorph >> treeLineDashes [
	^ treeLineDashes 
		ifNil: [treeLineDashes := self theme treeLineDashes]
	

]

{ #category : #accessing }
MorphTreeMorph >> treeLineDashes: anArrayOfInteger [
	treeLineDashes := anArrayOfInteger
	

]

{ #category : #geometry }
MorphTreeMorph >> treeLineWidth [
	^ treeLineWidth ifNil: [treeLineWidth := self  theme treeLineWidth] 
]

{ #category : #geometry }
MorphTreeMorph >> treeLineWidth: anInteger [
	treeLineWidth := anInteger
]

{ #category : #updating }
MorphTreeMorph >> update: aSymbol [
	aSymbol = self nodeListSelector
		ifTrue: [ ^ self updateList ].
	super update: aSymbol 
]

{ #category : #updating }
MorphTreeMorph >> updateColumnMorphs [
	self privateUpdateColumnMorphs

]

{ #category : #updating }
MorphTreeMorph >> updateColumnMorphsWidth [
	| rowMorphsWidths |
	self columns size > 1 ifFalse: [^ self].
	rowMorphsWidths := self rowMorphsWidths.
	self allNodeMorphs do: [:i | i updateColumnMorphsWidthWith: rowMorphsWidths].

]

{ #category : #updating }
MorphTreeMorph >> updateColumnResizersHeight [
	self columnResizers do: [:col | | b |
		b :=  col bounds.
		b := b left @ self top corner: b right @ scroller bottom.
		col bounds: b].

]

{ #category : #updating }
MorphTreeMorph >> updateColumnResizersXOffset [
	self columnResizerFrames with: self columnResizers do: [:frm :resizer | resizer bounds: (frm withBottom: scroller bottom)].

]

{ #category : #updating }
MorphTreeMorph >> updateContentsWithPreviouslyExpanded: aNodeList [
	"Make sure we notice the removal of the contained nodes."

	nodeList := nil.
	self noteRemovalOfAll: self allNodeMorphs.	"<-- Changed"
	(self nodeList isNil or: [ self nodeList isEmpty ])
		ifTrue: [ 
			nodeList := nil.
			^ self emptySelection ].
	self addSubmorphsFromNodeList: self currentNodelist previouslyExpanded: aNodeList.
	self listManager updateLastClickedMorph
]

{ #category : #updating }
MorphTreeMorph >> updateFromSelection: aSelection [
	aSelection selectedNodePathList do: [:path | self selectNodePath: path].
	self scrollSelectionIntoView
]

{ #category : #updating }
MorphTreeMorph >> updateList [
	|value|
	value := scrollBar value.
	self updateContentsWithPreviouslyExpanded: self currentlyExpanded.
	self vScrollValue: value
]

{ #category : #updating }
MorphTreeMorph >> updateSelectionFromModel [
	^ self listManager updateSelectionFromModel
]

{ #category : #updating }
MorphTreeMorph >> updateTopHeader [
	self topHeader ifNotNil: [:th | | w |
		th width: scroller width.
		w := self headerBounds.
		th submorphsDo: [:sm | w ifNotEmpty: [sm bounds: w removeFirst; layoutInset: self columnInset @ 0]]].

]

{ #category : #geometry }
MorphTreeMorph >> vExtraScrollRange [
	"Return the amount of extra blank space to include below the bottom of the scroll content."
	"The classic behavior would be ^bounds height - (bounds height * 3 // 4)"
	"Takes into accound the top header height if present"
	^ super vExtraScrollRange + self topHeaderHeight 

]

{ #category : #'dropping/grabbing' }
MorphTreeMorph >> wantsDroppedMorph: aMorph event: anEvent [
	"Return true if the receiver wishes to accept the given morph, which is being dropped 
	by a hand in response to the given event. Note that for a successful drop operation 
	both parties need to agree. The symmetric check is done automatically
	 via aMorph wantsToBeDroppedInto: self."

	^ self model wantsDroppedMorph: aMorph event: anEvent inMorph: self
]

{ #category : #accessing }
MorphTreeMorph >> withHLines [
	^ withHLines ifNil: [withHLines := false]
]

{ #category : #accessing }
MorphTreeMorph >> withHLines: aBoolean [
	withHLines := aBoolean
]

{ #category : #accessing }
MorphTreeMorph >> withTreeLines [
	^ self  treeLineWidth > 0
]

{ #category : #accessing }
MorphTreeMorph >> withTreeLines: aBoolean [
	self  treeLineWidth: 1
]

{ #category : #'events-processing' }
MorphTreeMorph >> yellowButtonEvent: anEvent [

	(self scrollerSubMorphFromPoint: anEvent position) 
		ifNotNil: [:sel |
			sel selected
				ifFalse: [self listManager setSelectedMorph: sel].
			^ self yellowButtonActivity: anEvent shiftPressed ].
	^ self yellowButtonActivity: anEvent shiftPressed
]
