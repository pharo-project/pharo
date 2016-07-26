recording
invalidRectsFullBounds: aRectangle
	"Return a collection of damaged rectangles for the given canvas. If a total repaint has been requested, return the given rectangle."

	^ totalRepaint
		ifTrue: [ Array with: aRectangle]
		ifFalse: [ invalidRects copy].

