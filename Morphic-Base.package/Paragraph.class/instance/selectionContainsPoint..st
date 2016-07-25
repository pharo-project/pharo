selection
selectionContainsPoint: aPoint
	"return whether the current selection contains the given point"
	^ self selectionRects anySatisfy: [ :rect| rect containsPoint: aPoint ]