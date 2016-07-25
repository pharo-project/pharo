display
displayExtraSelectionOn: aCanvas 
	"Send all visible lines to the displayScanner for display"

	| visibleRectangle line |
	visibleRectangle := aCanvas clipRect.
	refreshExtraSelection = true 
		ifTrue: [self buildSelectionBlocksFrom: visibleRectangle topLeft to: visibleRectangle bottomRight.
			refreshExtraSelection := false].
	extraSelectionBlocks ifNotNil: [
		(self lineIndexForPoint: visibleRectangle topLeft)
			to: (self lineIndexForPoint: visibleRectangle bottomRight)
			do: [:i | line := lines at: i.
				extraSelectionBlocks
					do: [:selblock | self displaySelectionBlock: selblock inLine: line on: aCanvas]]]