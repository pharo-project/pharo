private debug
isVisibleDebug: aRectangle
	"Return true if the given rectangle is (partially) visible"

	| vis canvasLocalClip |
	canvasLocalClip := Rectangle
		encompassing:
			(self clipRect corners
				collect: [ :c | (self canvas pathTransform transform: (c translateBy: origin)) translateBy: origin negated ]).	"canvasLocalClip := (self canvas pathTransform inverseTransform: aRectangle origin)
		corner: (self canvas pathTransform inverseTransform: aRectangle corner)."
	vis := self clipRect intersects: aRectangle.
	"self frameRectangle: aRectangle width: 1 color: Color red.
	self frameRectangle: self clipRect width: 1 color: Color green.
	self frameRectangle: canvasLocalClip width: 1 color: Color yellow."
	^ vis