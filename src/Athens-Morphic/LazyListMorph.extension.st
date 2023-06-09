Extension { #name : #LazyListMorph }

{ #category : #'*Athens-Morphic' }
LazyListMorph >> athensDisplay: item atRow: row on: aCanvas [
	"Display the given item at the given row on the given
	canvas."

	| c backgroundColor drawBounds |

	drawBounds := self drawBoundsForRow: row.
	c := self colorForRow: row.
	backgroundColor := self backgroundColorForRow: row.
	item listRenderOn: aCanvas asCanvasWrapper
		atRow: row
		bounds: drawBounds
		color: c
		backgroundColor: backgroundColor
		from: self.
	row = self mouseDownRow
		ifTrue: [
			(aCanvas setStrokePaint: c)
				width: 1;
				dashes: {1} offset: 0.
			aCanvas drawShape: (self selectionFrameForRow: row) ]
]

{ #category : #'*Athens-Morphic' }
LazyListMorph >> athensDrawBackgroundForRow: row on: aCanvas color: aColor [

	aCanvas setPaint: aColor.
	aCanvas drawShape: (self selectionFrameForRow: row)
]

{ #category : #'*Athens-Morphic' }
LazyListMorph >> athensDrawBackgroundForSearchedRow: row on: aCanvas [

	aCanvas setPaint: listSource secondarySelectionColor.
	aCanvas drawShape: (self selectionFrameForRow: row)
]

{ #category : #'*Athens-Morphic' }
LazyListMorph >> athensDrawBackgroundForSelectedRow: row on: aCanvas [

	aCanvas setPaint: listSource selectionColorToUse.
	aCanvas drawShape: (self selectionFrameForRow: row)
]

{ #category : #'*Athens-Morphic' }
LazyListMorph >> athensDrawSelectionOn: anAthensCanvas [

	selectedRow ifNil: [ ^self ].
	selectedRow = 0 ifTrue: [ ^self ].
	self athensDrawBackgroundForSelectedRow: selectedRow on: anAthensCanvas
]

{ #category : #'*Athens-Morphic' }
LazyListMorph >> athensHighlightPotentialDropRow: row on: anAthensCanvas [
	| drawBounds |
	drawBounds := self drawBoundsForRow: row.
	drawBounds := drawBounds intersect: self bounds ifNone: [ ^ self ].
	anAthensCanvas setStrokePaint: Color blue.
	anAthensCanvas drawShape: drawBounds
]

{ #category : #'*Athens-Morphic' }
LazyListMorph >> drawOnAthensCanvas: aCanvas [
	listItems isEmpty
		ifTrue: [^ self].
	"self drawSelectionOn: aCanvas."
	1 to: listItems size do: [:row |
		(listSource itemSelectedAmongMultiple: row)
			ifTrue: [ self athensDrawBackgroundForSelectedRow: row on: aCanvas]
			ifFalse: [
				(listSource searchedElement = row)
					ifTrue: [ self athensDrawBackgroundForSearchedRow: row on: aCanvas]
					ifFalse: [
						(listSource backgroundColorFor: row)
							ifNotNil: [:col |
								self athensDrawBackgroundForRow: row on: aCanvas color: col ]]].

			selectedRow = row ifTrue: [ self athensDrawSelectionOn: aCanvas ].
			(listSource separatorAfterARow: row) ifTrue: [ self athensDrawSeparatorAfter: row on: aCanvas ].
			self
				athensDisplay: (self item: row)
				atRow: row
				on: aCanvas].

	listSource potentialDropRow > 0
		ifTrue: [self athensHighlightPotentialDropRow: listSource potentialDropRow on: aCanvas]
]
