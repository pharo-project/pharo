*Polymorph-Widgets
focusChanged
	"Report that the area occupied by the morph's focus indicator should be redrawn.
	Optimized for border-only (no fill)."
	
	|rects fm|
	fm := self focusIndicatorMorph.
	fm fillStyle isTransparent
		ifTrue: [fm borderWidth > 0 ifTrue: [
					rects := fm bounds areasOutside: (fm bounds insetBy: fm borderWidth).
					rects do: [:r | self invalidRect: r]]]
		ifFalse: [self invalidRect: fm bounds]