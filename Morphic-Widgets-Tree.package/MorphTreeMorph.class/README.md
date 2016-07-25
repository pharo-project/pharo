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
PackageTreeExample new open. "For row coloring"
ClassListExample new openOn: Collection. "For column coloring"
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
