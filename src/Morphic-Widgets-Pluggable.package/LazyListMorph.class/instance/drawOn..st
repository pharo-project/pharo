drawing
drawOn: aCanvas 
	listItems isEmpty
		ifTrue: [^ self].
		
	aCanvas fillRectangle: aCanvas clipRect color: (self theme listNormalFillStyleFor: self).
		
	"self drawSelectionOn: aCanvas."
	(self topVisibleRowForCanvas: aCanvas)
		to: (self bottomVisibleRowForCanvas: aCanvas) 
		do: [:row | 
			
			(listSource itemSelectedAmongMultiple: row)
				ifTrue: [ self drawBackgroundForSelectedRow: row on: aCanvas]
				ifFalse: [
					(listSource searchedElement = row)
						ifTrue: [ self drawBackgroundForSearchedRow: row on: aCanvas]
						ifFalse: [
							(listSource backgroundColorFor: row)
								ifNotNil: [:col |
									self drawBackgroundForRow: row on: aCanvas color: col ]]].
						
			selectedRow = row ifTrue: [ self drawSelectionOn: aCanvas ].
			(listSource separatorAfterARow: row) ifTrue: [ self drawSeparatorAfter: row on: aCanvas ].
			self
				display: (self item: row)
				atRow: row
				on: aCanvas].
		
	listSource potentialDropRow > 0
		ifTrue: [self highlightPotentialDropRow: listSource potentialDropRow on: aCanvas]