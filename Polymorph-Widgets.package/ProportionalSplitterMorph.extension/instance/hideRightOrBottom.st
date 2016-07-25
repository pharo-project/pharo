*Polymorph-Widgets
hideRightOrBottom
	"Hide the receiver and all right or bottom morphs."

	self hide.
	rightOrBottom do: [:m | m hide]