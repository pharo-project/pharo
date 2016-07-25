*Polymorph-Widgets
showLeftOrTop
	"Show the receiver and all left or top morphs."

	self show.
	leftOrTop do: [:m | m show]