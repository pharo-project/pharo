accessing
selectionRects
	"Return an array of rectangles representing the selection region."

	^ self selectionStart
		ifNil: [ Array new ]
		ifNotNil: [ self selectionRectsFrom: self selectionStart to: self selectionStop ]