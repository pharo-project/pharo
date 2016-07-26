display
displayOn: aCanvas using: displayScanner at: somePosition
	"Send all visible lines to the displayScanner for display"
	| visibleRectangle offset leftInRun line |
	visibleRectangle := aCanvas clipRect.
	offset := (somePosition - positionWhenComposed) truncated.
	leftInRun := 0.
	(self lineIndexForPoint: visibleRectangle topLeft)
		to: (self lineIndexForPoint: visibleRectangle bottomRight)
		do: [:i | line := lines at: i.
			self displaySelectionInLine: line on: aCanvas.
			line first <= line last ifTrue:
				[leftInRun := displayScanner displayLine: line offset: offset leftInRun: leftInRun]].
