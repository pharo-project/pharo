*Athens-Morphic
drawOnAthensCanvas: aCanvas
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