halo commands
alignTopEdges
	"Make the top coordinate of all my elements be the same"

	| minTop |
	minTop := (selectedItems collect: [:itm | itm top]) min.
	selectedItems do:
		[:itm | itm top: minTop].

	self changed
