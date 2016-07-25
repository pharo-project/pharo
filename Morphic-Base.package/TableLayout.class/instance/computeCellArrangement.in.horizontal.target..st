nil
computeCellArrangement: cellHolder in: newBounds horizontal: aBool target: aMorph
	"Compute number of cells we can put in each row/column. The returned array contains a list of all the cells we can put into the row/column at each level.
	Note: The arrangement is so that the 'x' value of each cell advances along the list direction and the 'y' value along the wrap direction. The returned arrangement has an extra cell at the start describing the width and height of the row."
	| cells wrap spacing output maxExtent n sum index max cell first last w cellMax maxCell hFill vFill inset |
	maxCell := cellHolder key.
	cells := cellHolder value.
	properties wrapDirection == #none 
		ifTrue:[wrap := SmallInteger maxVal]
		ifFalse:[wrap := aBool ifTrue:[newBounds width] ifFalse:[newBounds height].
				wrap < maxCell x ifTrue:[wrap := maxCell x]].
	spacing := properties cellSpacing.
	(spacing == #globalRect or:[spacing = #globalSquare]) ifTrue:[
		"Globally equal spacing is a very special case here, so get out fast and easy"
		^self computeGlobalCellArrangement: cells 
			in: newBounds horizontal: aBool 
			wrap: wrap spacing: spacing].

	output := Array new writeStream.
	inset := properties cellInset asPoint.
	aBool ifFalse:[inset := inset transposed].
	first := last := nil.
	maxExtent := 0@0.
	sum := 0.
	index := 1.
	n := 0.
	hFill := vFill := false.
	[index <= cells size] whileTrue:[
		w := sum.
		cell := cells at: index.
		cellMax := maxExtent max: cell cellSize. "e.g., minSize"
		(spacing == #localRect or:[spacing == #localSquare]) ifTrue:[
			"Recompute entire size of current row"
			spacing == #localSquare 
				ifTrue:[max := cellMax x max: cellMax y]
				ifFalse:[max := cellMax x].
			sum := (n + 1) * max.
		] ifFalse:[
			sum := sum + (cell cellSize x).
		].
		((sum + (n * inset x)) > wrap and:[first notNil]) ifTrue:[
			"It doesn't fit and we're not starting a new line"
			(spacing == #localSquare or:[spacing == #localRect]) ifTrue:[
				spacing == #localSquare 
					ifTrue:[maxExtent := (maxExtent x max: maxExtent y) asPoint].
				first do:[:c| c cellSize: maxExtent]].
			w := w + ((n - 1) * inset x).
			"redistribute extra space"
			first nextCell ifNotNil:[first nextCell do:[:c| c addExtraSpace: inset x@0]].
			last := LayoutCell new.
			last cellSize: w @ (maxExtent y).
			last hSpaceFill: hFill.
			last vSpaceFill: vFill.
			last nextCell: first.
			output position = 0 ifFalse:[last addExtraSpace: 0@inset y].
			output nextPut: last.
			first := nil.
			maxExtent := 0@0.
			sum := 0.
			n := 0.
			hFill := vFill := false.
		] ifFalse:[
			"It did fit; use next item from input"
			first ifNil:[first := last := cell] ifNotNil:[last nextCell: cell. last := cell].
			index := index+1.
			n := n + 1.
			maxExtent := cellMax.
			hFill := hFill or:[cell hSpaceFill].
			vFill := vFill or:[cell vSpaceFill].
		].
	].
	first ifNotNil:[
		last := LayoutCell new.
		sum := sum + ((n - 1) * inset x).
		first nextCell ifNotNil:[first nextCell do:[:c| c addExtraSpace: inset x@0]].
		last cellSize: sum @ maxExtent y.
		last hSpaceFill: hFill.
		last vSpaceFill: vFill.
		last nextCell: first.
		output position = 0 ifFalse:[last addExtraSpace: 0@inset y].
		output nextPut: last].
	output := output contents.
	properties listSpacing == #equal ifTrue:[
		"Make all the heights equal"
		max := output inject: 0 into:[:size :c| size max: c cellSize y].
		output do:[:c| c cellSize: c cellSize x @ max].
	].
	^output