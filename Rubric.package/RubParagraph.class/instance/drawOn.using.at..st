drawing
drawOn: aCanvas using: aDisplayScanner at: aPosition
	"Send all visible lines to the displayScanner for display"

	| offset leftInRun line visibleRectangle |
	self drawingEnabled
		ifFalse: [ ^ self ].
	visibleRectangle := aCanvas clipRect.
	offset := (aPosition - self position) truncated.
	leftInRun := 0.
	(self lineIndexForPoint: visibleRectangle topLeft) to: (self lineIndexForPoint: visibleRectangle bottomRight) do: [ :i | 
		line := self lines at: i.
		line first <= line last
			ifTrue: [ leftInRun := aDisplayScanner displayLine: line offset: offset leftInRun: leftInRun ] ]