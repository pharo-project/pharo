editing
preferredKeyboardPosition

	| default rects |
	default  := (self bounds: self bounds in: World) topLeft.
	paragraph ifNil: [^ default].
	rects := paragraph selectionRects.
	rects size = 0 ifTrue: [^ default].
	^ rects first topLeft.

	"^ (self bounds: self bounds in: World) topLeft."
