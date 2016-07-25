selection
selectionRects
	"Return an array of rectangles representing the selection region."
	selectionStart ifNil: [^ Array new].
	^ self selectionRectsFrom: selectionStart to: selectionStop