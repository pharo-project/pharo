layout
minExtentOf: aMorph in: box 
	"Return the minimal size aMorph's children would require given the new bounds"

	| cells arrangement horizontal newBounds minX minY dir |
	minExtentCache isNil ifFalse: [^minExtentCache].
	aMorph hasSubmorphs ifFalse: [^0 @ 0].
	properties := aMorph assureTableProperties.
	(properties wrapDirection == #none and: [properties cellSpacing == #none]) 
		ifTrue: 
			["Get into the fast lane"

			dir := properties listDirection.
			(dir == #leftToRight or: [dir == #rightToLeft]) 
				ifTrue: [^self minExtentHorizontal: aMorph].
			(dir == #topToBottom or: [dir == #bottomToTop]) 
				ifTrue: [^self minExtentVertical: aMorph]].
	newBounds := box origin asIntegerPoint corner: box corner asIntegerPoint.
	horizontal := (properties listDirection == #topToBottom 
				or: [properties listDirection == #bottomToTop]) not.
	"Step 1: Compute the minimum extent for all the children of aMorph"
	cells := self 
				computeCellSizes: aMorph
				in: (0 @ 0 corner: newBounds extent)
				horizontal: horizontal.
	"Step 2: Compute the arrangement of the cells for each row and column"
	arrangement := self 
				computeCellArrangement: cells
				in: newBounds
				horizontal: horizontal
				target: aMorph.
	"Step 3: Extract the minimum size out of the arrangement"
	minX := minY := 0.
	arrangement do: 
			[:cell | 
			minX := minX max: cell cellSize x + cell extraSpace x.
			minY := minY + cell cellSize y + cell extraSpace y].
	minExtentCache := horizontal ifTrue: [minX @ minY] ifFalse: [minY @ minX].
	^minExtentCache