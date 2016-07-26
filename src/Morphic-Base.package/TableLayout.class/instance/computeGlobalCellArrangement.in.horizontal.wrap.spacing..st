layout
computeGlobalCellArrangement: cells in: newBounds horizontal: aBool wrap: wrap spacing: spacing
	"Compute number of cells we can put in each row/column. The returned array contains a list of all the cells we can put into the row/column at each level.
	Note: The arrangement is so that the 'x' value of each cell advances along the list direction and the 'y' value along the wrap direction. The returned arrangement has an extra cell at the start describing the width and height of the row."
	| output maxExtent n cell first last hFill vFill |
	output := Array new writeStream.
	first := last := nil.
	maxExtent := cells inject: 0@0 into:[:size :c| size max: c cellSize "e.g., minSize"].
	spacing == #globalSquare ifTrue:[maxExtent := (maxExtent x max: maxExtent y) asPoint].
	n := (wrap // maxExtent x) max: 1.
	hFill := vFill := false.
	1 to: cells size do:[:i|
		cell := cells at: i.
		hFill := hFill or:[cell hSpaceFill].
		vFill := vFill or:[cell vSpaceFill].
		cell cellSize: maxExtent.
		first ifNil:[first := last := cell] ifNotNil:[last nextCell: cell. last := cell].
		(i \\ n) = 0 ifTrue:[
			last := LayoutCell new.
			last cellSize: (maxExtent x * n) @ (maxExtent y).
			last hSpaceFill: hFill.
			last vSpaceFill: vFill.
			hFill := vFill := false.
			last nextCell: first.
			output nextPut: last.
			first := nil]].
	first ifNotNil:[
		last := LayoutCell new.
		last cellSize: (maxExtent x * n) @ (maxExtent y). self flag: #arNote."@@@: n is not correct!"
		last nextCell: first.
		output nextPut: last].
	^output contents
