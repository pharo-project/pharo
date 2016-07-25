*Polymorph-Widgets
hideLeftOrTop
	"Hide the receiver and all left or top morphs."

	self hide.
	leftOrTop do: [:m | m hide]