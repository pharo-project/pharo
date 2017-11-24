"
A variant of LazyListMorph that can display multi-column lists.
"
Class {
	#name : #MulticolumnLazyListMorph,
	#superclass : #LazyListMorph,
	#instVars : [
		'columnWidths'
	],
	#category : #'Morphic-Widgets-Pluggable'
}

{ #category : #'row management' }
MulticolumnLazyListMorph >> display: items atRow: row on: canvas [
	"display the specified item, which is on the specified row; for Multicolumn 
	lists, items will be a list of strings"

	| drawBounds backgroundColor |
	backgroundColor := self backgroundColorForRow: row.
	drawBounds := self drawBoundsForRow: row.
	drawBounds := drawBounds
		intersect: self bounds
		ifNone: [ 
			"oh well"
			^ self ].
	items
		with: (1 to: items size)
		do: [ :item :index | 
			"move the bounds to the right at each step"
			index > 1
				ifTrue: [ drawBounds := drawBounds left: drawBounds left + listSource gapSize + (columnWidths at: index - 1) ].
			item
				listRenderOn: canvas
				atRow: row
				bounds: drawBounds
				color: color
				backgroundColor: backgroundColor
				from: self ]
]

{ #category : #'as yet unclassified' }
MulticolumnLazyListMorph >> drawBoundsForRow: row [
	"calculate the bounds that row should be drawn at.  This might be outside our bounds!"
	| topLeft drawBounds item width height |
	
	item := self getListItem: row.
	height := (item collect: [:e | e heightToDisplayInList: self ]) max.
	width := self width.
		
	topLeft := self topLeft x @ (self topLeft y + ((row - 1) * (height))).
	drawBounds := topLeft extent: (width @ height).
	^drawBounds
]

{ #category : #drawing }
MulticolumnLazyListMorph >> drawOn: aCanvas [
	self getListSize = 0 ifTrue:[ ^self ].

	self setColumnWidthsFor: aCanvas.
	self adjustWidth.
	
	super drawOn: aCanvas
]

{ #category : #'list access' }
MulticolumnLazyListMorph >> getListItem: index [
	^listSource getListRow: index
]

{ #category : #'scroll range' }
MulticolumnLazyListMorph >> hUnadjustedScrollRange [
"bvr - Introduce here the old version of the super method just waiting for this class to be deleted"

	| itemsToCheck item index |
	"Check for a cached value"
	maxWidth ifNotNil:[^maxWidth].

	listItems isEmpty ifTrue: [^0]. "don't set maxWidth if empty do will be recomputed when there are some items"
	"Compute from scratch"
	itemsToCheck := 30 min: (listItems size).
	maxWidth := 0. 

	"Check the first few items to get a representative sample of the rest of the list."
	index := 1.
	[index < itemsToCheck] whileTrue:
		[ item := self getListItem: index. "Be careful not to actually install this item"
		maxWidth := maxWidth max: (self widthToDisplayItem: item).
		index:= index + 1.
		].

	"Add some initial fudge if we didn't check all the items."
	(itemsToCheck < listItems size) ifTrue:[maxWidth := maxWidth*2].

	^maxWidth + 150

]

{ #category : #'list access' }
MulticolumnLazyListMorph >> item: index [
	"return the index-th item, using the 'listItems' cache"
	| newItem itemWidth |
	(index between: 1 and: listItems size)
		ifFalse: [ "there should have been an update, but there wasn't!"  ^self getListItem: index].
	(listItems at: index) ifNil: [ 
		newItem := self getListItem: index.

		maxWidth ifNotNil:[
			itemWidth := self widthToDisplayItem: newItem.
			itemWidth > maxWidth ifTrue:[
				maxWidth := itemWidth.
				self adjustWidth.
			]].
		listItems at: index put: newItem ].
	^listItems at: index
]

{ #category : #'row management' }
MulticolumnLazyListMorph >> listChanged [
	columnWidths := nil.
	super listChanged
]

{ #category : #drawing }
MulticolumnLazyListMorph >> setColumnWidthsFor: aCanvas [
        | row topRow bottomRow |
        "set columnWidths for drawing on the specified canvas"
		columnWidths ifNil: [
		columnWidths := (self item: 1) collect: [ :ignored | 0 ]. ].
	topRow := (self topVisibleRowForCanvas: aCanvas) max: 1.
	bottomRow :=  (self bottomVisibleRowForCanvas: aCanvas) max: 1.
	topRow > bottomRow ifTrue: [ ^ self ].
	topRow to: bottomRow do: [ :rowIndex |
                row := self item: rowIndex.
                columnWidths := columnWidths with: row collect: [ :currentWidth :item |
				| widthOfItem |
				widthOfItem := (font widthOfStringOrText: item).
				widthOfItem > currentWidth
					ifTrue: [ self changed.  widthOfItem ]
					ifFalse: [ currentWidth ] ] ]
]

{ #category : #'scroll range' }
MulticolumnLazyListMorph >> widthToDisplayItem: item [
	"This class will be removed soon, so this method will disappear"
	| widths |
	
	widths := item collect: [ :each | each widthToDisplayInList: self ].
	^widths sum + ((listSource gapSize + 4) * (widths size - 1))   "add in space between the columns"

]
