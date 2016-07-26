*Polymorph-Widgets
selectionFrame
	"Answer the selection frame rectangle."

	|frame|
	frame := self bounds: self bounds in: container.
	frame := self
		bounds: ((frame left: container innerBounds left) right: container innerBounds right)
		from: container.
	^frame