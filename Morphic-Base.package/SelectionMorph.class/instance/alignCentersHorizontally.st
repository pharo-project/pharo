halo commands
alignCentersHorizontally
	"Make every morph in the selection have the same vertical center as the topmost item."

	| minLeft leftMost |
	selectedItems size > 1 ifFalse: [^ self].
	minLeft := (selectedItems collect: [:itm | itm left]) min.
	leftMost := selectedItems detect: [:m | m left = minLeft].
	selectedItems do:
		[:itm | itm center: (itm center x @ leftMost center y)].

	self changed
