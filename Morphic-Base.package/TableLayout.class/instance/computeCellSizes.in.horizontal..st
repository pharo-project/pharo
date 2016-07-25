layout
computeCellSizes: aMorph in: newBounds horizontal: aBool
	"Step 1: Compute the minimum extent for all the children of aMorph"
	| cells block minSize maxSize maxCell |
	cells := (Array new: aMorph submorphCount) writeStream.
	minSize := properties minCellSize asPoint.
	maxSize := properties maxCellSize asPoint.
	aBool ifTrue:[
		minSize := minSize transposed.
		maxSize := maxSize transposed].
	maxCell := 0@0.
	block := [:m| | cell size |
		m disableTableLayout ifFalse:[
			size := m minExtent asIntegerPoint.
			cell := LayoutCell new target: m.
			aBool ifTrue:[
				cell hSpaceFill: m hResizing == #spaceFill.
				cell vSpaceFill: m vResizing == #spaceFill.
			] ifFalse:[
				cell hSpaceFill: m vResizing == #spaceFill.
				cell vSpaceFill: m hResizing == #spaceFill.
				size := size transposed.
			].
			size := (size min: maxSize) max: minSize.
			cell cellSize: size.
			maxCell := maxCell max: size.
			cells nextPut: cell]].
	properties reverseTableCells
		ifTrue:[aMorph submorphsReverseDo: block]
		ifFalse:[aMorph submorphsDo: block].
	^maxCell -> cells contents