*Polymorph-Widgets
showRightOrBottom
	"Show the receiver and all right or bottom morphs."

	self show.
	rightOrBottom do: [:m | m show]