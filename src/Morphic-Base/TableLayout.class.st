"
The layout process:
For computing the new layout for the children of any morph, we start with an initial rectangle which is provided as a reference.

Step 1: The first step of layout computation is to compute the minimum extent each of our children can have. The minimum extent is mapped through both the local layout frame of the morph (for relative positioning) and the global layout frame (for insets, such as cursor indication) to obtain the minimal size required for each cell.

Step 2: Based on the cell sizes, the number of cells we can put into each row and column is computed. For equal spacing, the maximum size of the cells is taken into account here.

Step 3: Based on the row/column sizes, we compute the extra space which should be added to each row/column. For 
	#leftFlush/#topFlush - we add all extra space add the end
	#rightFlush/#bottomFlush - we add all extra space at the start
	#centering - we add 1/2 of the extra space at start and end
	#justified - we distribute the space evenly between the morphs
[NOTE: If any #spaceFill morphs are encountered during this step, #justified is implied and the space is exclusively and equally distributed between those #spaceFill morphs. This is for backward compatibility and should *never* be necessary in the new regime].

Step 4: The morphs are placed in the computed cells and the extra space is distributed as necessary. Placing the submorphs is done by mapping through the global and the local layout frame as requested.

Start point:
=> bounds: new rectangle for the morph.

Compute basic arrangement of morphs:
=> For each submorph compute minExtent
	- if global layout frame inset in global layout frame
	- if local layout frame inset in local layout frame
=> Compute number of morphs per, width and height of row/column
	- if equal spacing based on max size
=> Compute extra space per row/column
	- if centering = #justified; distribute space equally
	- if centering #leftFlush/#topFlush (-1) add start extra
	- if centering #rightFlush/#bottomFlush (1) add end extra
	- if centering #centered add 1/2 extra to start/end
	<extra space must be float and rounded accordingly!>
=> Place morphs in appropriate cells
	- if global layout frame inset in global layout frame
	- if local layout frame inset in local layout frame
	<will likely cause #layoutChanged by submorphs>

Distribute morphs in row/column:

=> Compute the max length of each row/column

"
Class {
	#name : #TableLayout,
	#superclass : #LayoutPolicy,
	#instVars : [
		'properties',
		'minExtentCache'
	],
	#category : #'Morphic-Base-Layouts'
}

{ #category : #nil }
TableLayout >> computeCellArrangement: cellHolder in: newBounds horizontal: aBool target: aMorph [
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
]

{ #category : #layout }
TableLayout >> computeCellSizes: aMorph in: newBounds horizontal: aBool [
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
]

{ #category : #layout }
TableLayout >> computeExtraSpacing: arrangement in: newBounds horizontal: aBool target: aMorph [ 
	"Compute the required extra spacing for laying out the cells"

	"match newBounds extent with arrangement's orientation"

	| extent extra centering n extraPerCell cell last hFill vFill max amount allow |
	extent := newBounds extent.
	aBool ifFalse: [extent := extent transposed].

	"figure out if we have any horizontal or vertical space fillers"
	hFill := vFill := false.
	max := 0 @ 0.
	arrangement do: 
			[:c | 
			max := (max x max: c cellSize x) @ (max y + c cellSize y).
			max := max max: c cellSize.
			hFill := hFill or: [c hSpaceFill].
			vFill := vFill or: [c vSpaceFill]].

	"Take client's shrink wrap constraints into account.
	Note: these are only honored when there are no #spaceFill children,
	or when #rubberBandCells is set."
	allow := properties rubberBandCells not.
	aMorph hResizing == #shrinkWrap 
		ifTrue: 
			[aBool 
				ifTrue: [allow & hFill ifFalse: [extent := max x @ (max y max: extent y)]]
				ifFalse: [allow & vFill ifFalse: [extent := (max x max: extent x) @ max y]]].
	aMorph vResizing == #shrinkWrap 
		ifTrue: 
			[aBool 
				ifFalse: [allow & hFill ifFalse: [extent := max x @ (max y max: extent y)]]
				ifTrue: [allow & vFill ifFalse: [extent := (max x max: extent x) @ max y]]].

	"Now compute the extra v space"
	extra := extent y 
				- (arrangement inject: 0 into: [:sum :c | sum + c cellSize y]).
	extra > 0 
		ifTrue: 
			["Check if we have any #spaceFillers"

			vFill 
				ifTrue: 
					["use only #spaceFillers"

					n := arrangement inject: 0
								into: [:sum :c | c vSpaceFill ifTrue: [sum + 1] ifFalse: [sum]].
					n isZero ifFalse: [extraPerCell := extra asFloat / n asFloat].
					extra := last := 0.
					arrangement do: 
							[:c | 
							c vSpaceFill 
								ifTrue: 
									[extra := (last := extra) + extraPerCell.
									amount := 0 @ (extra truncated - last truncated).
									c do: [:cc | cc cellSize: cc cellSize + amount]]]]
				ifFalse: 
					["no #spaceFillers; distribute regularly"

					centering := properties wrapCentering.
					"centering == #topLeft ifTrue:[]."	"add all extra space to the last cell; e.g., do nothing"
					centering == #bottomRight 
						ifTrue: 
							["add all extra space to the first cell"

							arrangement first addExtraSpace: 0 @ extra].
					centering == #center 
						ifTrue: 
							["add 1/2 extra space to the first and last cell"

							arrangement first addExtraSpace: 0 @ (extra // 2)].
					centering == #justified 
						ifTrue: 
							["add extra space equally distributed to each cell"

							n := arrangement size - 1 max: 1.
							extraPerCell := extra asFloat / n asFloat.
							extra := last := 0.
							arrangement do: 
									[:c | 
									c addExtraSpace: 0 @ (extra truncated - last truncated).
									extra := (last := extra) + extraPerCell]]]].

	"Now compute the extra space for the primary direction"
	centering := properties listCentering.
	1 to: arrangement size
		do: 
			[:i | 
			cell := arrangement at: i.
			extra := extent x - cell cellSize x.
			extra > 0 
				ifTrue: 
					["Check if we have any #spaceFillers"
					cell := cell nextCell.
					cell hSpaceFill 
						ifTrue: 
							["use only #spaceFillers"

							
							n := cell inject: 0
										into: [:sum :c | c hSpaceFill ifTrue: [sum + c target spaceFillWeight] ifFalse: [sum]].
							n isZero ifFalse: [extraPerCell := extra asFloat / n asFloat].
							extra := last := 0.
							cell do: 
									[:c | 
									c hSpaceFill 
										ifTrue: 
											[extra := (last := extra) + (extraPerCell * c target spaceFillWeight).
											amount := extra truncated - last truncated.
											c cellSize: c cellSize + (amount @ 0)]]]
						ifFalse: 
							["no #spaceFiller; distribute regularly"

						
							"centering == #topLeft ifTrue:[]"	"add all extra space to the last cell; e.g., do nothing"
							centering == #bottomRight 
								ifTrue: 
									["add all extra space to the first cell"

									cell addExtraSpace: extra @ 0].
							centering == #center 
								ifTrue: 
									["add 1/2 extra space to the first and last cell"

									cell addExtraSpace: (extra // 2) @ 0].
							centering == #justified 
								ifTrue: 
									["add extra space equally distributed to each cell"

									n := cell size - 1 max: 1.
									extraPerCell := extra asFloat / n asFloat.
									extra := last := 0.
									cell do: 
											[:c | 
											c addExtraSpace: (extra truncated - last truncated) @ 0.
											extra := (last := extra) + extraPerCell]]]]]
]

{ #category : #layout }
TableLayout >> computeGlobalCellArrangement: cells in: newBounds horizontal: aBool wrap: wrap spacing: spacing [
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
		last cellSize: (maxExtent x * n) @ (maxExtent y). self flag: #pharoFixMe."@@@: n is not correct!"
		last nextCell: first.
		output nextPut: last].
	^output contents

]

{ #category : #layout }
TableLayout >> flushLayoutCache [
	"Flush any cached information associated with the receiver"
	minExtentCache := nil.
]

{ #category : #utilities }
TableLayout >> indexForInserting: aMorph at: aPoint in: owner [ 
	"Return the insertion index based on the layout strategy defined for some morph. Used for drop insertion."

	| horizontal morphList index |
	owner hasSubmorphs ifFalse: [^1].
	aMorph disableTableLayout ifTrue: [^1].
	horizontal := (owner listDirection == #topToBottom 
				or: [owner listDirection == #bottomToTop]) not .
	morphList := owner submorphs.
	owner reverseTableCells ifTrue: [morphList := morphList reversed].
	index := self 
				indexForInserting: aPoint
				inList: morphList
				horizontal: horizontal
				target: owner.
	owner reverseTableCells ifTrue: [index := morphList size - index + 2].
	^index ifNil: [1]
]

{ #category : #utilities }
TableLayout >> indexForInserting: aPoint inList: morphList horizontal: aBool target: aMorph [ 
	| cmp1 cmp2 cmp3 noWrap |
	properties := aMorph layoutProperties.
	noWrap := properties wrapDirection == #none.
	aBool 
		ifTrue: 
			["horizontal"

			properties listDirection == #rightToLeft 
				ifTrue: [cmp1 := [:rect | aPoint x > rect left]]
				ifFalse: [cmp1 := [:rect | aPoint x < rect right]].
			properties wrapDirection == #bottomToTop 
				ifTrue: 
					[cmp2 := [:rect | aPoint y > rect top].
					cmp3 := [:rect | aPoint y > rect bottom]]
				ifFalse: 
					[cmp2 := [:rect | aPoint y < rect bottom].
					cmp3 := [:rect | aPoint y < rect top]]]
		ifFalse: 
			["vertical"

			properties listDirection == #bottomToTop 
				ifTrue: [cmp1 := [:rect | aPoint y > rect top]]
				ifFalse: [cmp1 := [:rect | aPoint y < rect bottom]].
			properties wrapDirection == #rightToLeft 
				ifTrue: 
					[cmp2 := [:rect | aPoint x > rect left].
					cmp3 := [:rect | aPoint x > rect right]]
				ifFalse: 
					[cmp2 := [:rect | aPoint x < rect right].
					cmp3 := [:rect | aPoint x < rect left]]]. 
	morphList keysAndValuesDo: 
			[:index :m | | box | 
			self flag: #pharoFixMe.	"it is not quite clear if we can really use #fullBounds here..."
			box := m fullBounds.
			noWrap 
				ifTrue: 
					["Only in one direction"

					(cmp1 value: box) ifTrue: [^index]]
				ifFalse: 
					["Check for inserting before current row"

					(cmp3 value: box) ifTrue: [^index].
					"Check for inserting before current cell"
					((cmp1 value: box) and: [cmp2 value: box]) ifTrue: [^index]]].
	^morphList size + 1
]

{ #category : #testing }
TableLayout >> isTableLayout [
	^true
]

{ #category : #layout }
TableLayout >> layout: aMorph in: box [ 
	"Compute the layout for the given morph based on the new bounds"

	| cells arrangement horizontal newBounds |
	aMorph hasSubmorphs ifFalse: [^self].
	properties := aMorph assureTableProperties.
	newBounds := box origin asIntegerPoint corner: box corner asIntegerPoint.
	(properties wrapDirection == #none and: [properties cellSpacing == #none]) 
		ifTrue: 
			["get into the fast lane"

			properties listCentering == #justified 
				ifFalse: 
					["can't deal with that"

					properties listDirection == #leftToRight 
						ifTrue: [^self layoutLeftToRight: aMorph in: newBounds].
					properties listDirection == #topToBottom 
						ifTrue: [^self layoutTopToBottom: aMorph in: newBounds]]].
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
	"Step 3: Compute the extra spacing for each cell"
	self 
		computeExtraSpacing: arrangement
		in: newBounds
		horizontal: horizontal
		target: aMorph.
	"Step 4: Place the children within the cells accordingly"
	self 
		placeCells: arrangement
		in: newBounds
		horizontal: horizontal
		target: aMorph
]

{ #category : #layout }
TableLayout >> layoutLeftToRight: aMorph in: newBounds [
	"An optimized left-to-right list layout"
	| inset extent block posX posY centering extraPerCell amount minX minY maxX maxY n width extra last cell size height sum vFill first |
	size := properties minCellSize asPoint. minX := size x. minY := size y.
	size := properties maxCellSize asPoint. maxX := size x. maxY := size y.
	inset := properties cellInset asPoint x.
	extent := newBounds extent.
	n := 0. vFill := false. sum := 0.
	width := height := 0.
	first := last := nil.
	block := [:m| | sizeY sizeX props |
		props := m layoutProperties ifNil:[m].
		props disableTableLayout ifFalse:[
			n := n + 1.
			cell := LayoutCell new target: m.
			(props hResizing == #spaceFill) ifTrue:[
				cell hSpaceFill: true.
				extra := m spaceFillWeight.
				cell extraSpace: extra.
				sum := sum + extra.
			] ifFalse:[cell hSpaceFill: false].
			(props vResizing == #spaceFill) ifTrue:[vFill := true].
			size := m minExtent. sizeX := size x. sizeY := size y.
			sizeX < minX
				ifTrue:[sizeX := minX]
				ifFalse:[sizeX > maxX ifTrue:[sizeX := maxX]].
			sizeY < minY
				ifTrue:[sizeY := minY]
				ifFalse:[sizeY > maxY ifTrue:[sizeY := maxY]].
			cell cellSize: sizeX.
			last ifNil:[first := cell] ifNotNil:[last nextCell: cell].
			last := cell.
			width := width + sizeX.
			sizeY > height ifTrue:[height := sizeY].
		].
	].
	properties reverseTableCells
		ifTrue:[aMorph submorphsReverseDo: block]
		ifFalse:[aMorph submorphsDo: block].

	n > 1 ifTrue:[width := width + (n-1 * inset)].

	(properties hResizing == #shrinkWrap and:[properties rubberBandCells or:[sum isZero]])
		ifTrue:[extent := width @ (extent y max: height)].
	(properties vResizing == #shrinkWrap and:[properties rubberBandCells or:[vFill not]])
		ifTrue:[extent := (extent x max: width) @ height].

	posX := newBounds left.
	posY := newBounds top.

	"Compute extra vertical space"
	extra := extent y - height.
	extra < 0 ifTrue:[extra := 0].
	extra > 0 ifTrue:[
		vFill ifTrue:[
			height := extent y.
		] ifFalse:[
			centering := properties wrapCentering.
			centering == #bottomRight ifTrue:[posY := posY + extra].
			centering == #center ifTrue:[posY := posY + (extra // 2)]
		].
	].


	"Compute extra horizontal space"
	extra := extent x - width.
	extra < 0 ifTrue:[extra := 0].
	extraPerCell := 0.
	extra > 0 ifTrue:[
		sum isZero ifTrue:["extra space but no #spaceFillers"
			centering := properties listCentering.
			centering == #bottomRight ifTrue:[posX := posX + extra].
			centering == #center ifTrue:[posX := posX + (extra // 2)].
		] ifFalse:[extraPerCell := extra asFloat / sum asFloat].
	].

	n := 0.
	extra := last := 0.
	cell := first.
	[cell == nil] whileFalse:[
		n := n + 1.
		width := cell cellSize.
		(extraPerCell > 0 and:[cell hSpaceFill]) ifTrue:[
			extra := (last := extra) + (extraPerCell * cell extraSpace).
			amount := extra truncated - last truncated.
			width := width + amount.
		].
		cell target layoutInBounds: (posX @ posY extent: width @ height).
		posX := posX + width + inset.
		cell := cell nextCell.
	].

]

{ #category : #nil }
TableLayout >> layoutTopToBottom: aMorph in: newBounds [
	"An optimized top-to-bottom list layout"
	| inset extent block posX posY centering extraPerCell amount minX minY maxX maxY n height extra last cell size width sum vFill first |
	size := properties minCellSize asPoint. minX := size x. minY := size y.
	size := properties maxCellSize asPoint. maxX := size x. maxY := size y.
	inset := properties cellInset asPoint y.
	extent := newBounds extent.
	n := 0. vFill := false. sum := 0.
	width := height := 0.
	first := last := nil.
	block := [:m| | sizeX props sizeY |
		props := m layoutProperties ifNil:[m].
		props disableTableLayout ifFalse:[
			n := n + 1.
			cell := LayoutCell new target: m.
			(props vResizing == #spaceFill) ifTrue:[
				cell vSpaceFill: true.
				extra := m spaceFillWeight.
				cell extraSpace: extra.
				sum := sum + extra.
			] ifFalse:[cell vSpaceFill: false].
			(props hResizing == #spaceFill) ifTrue:[vFill := true].
			size := m minExtent. sizeX := size x. sizeY := size y.
			sizeX < minX
				ifTrue:[sizeX := minX]
				ifFalse:[sizeX > maxX ifTrue:[sizeX := maxX]].
			sizeY < minY
				ifTrue:[sizeY := minY]
				ifFalse:[sizeY > maxY ifTrue:[sizeY := maxY]].
			cell cellSize: sizeY.
			first ifNil:[first := cell] ifNotNil:[last nextCell: cell].
			last := cell.
			height := height + sizeY.
			sizeX > width ifTrue:[width := sizeX].
		].
	].
	properties reverseTableCells
		ifTrue:[aMorph submorphsReverseDo: block]
		ifFalse:[aMorph submorphsDo: block].

	n > 1 ifTrue:[height := height + (n-1 * inset)].

	(properties vResizing == #shrinkWrap and:[properties rubberBandCells or:[sum isZero]])
		ifTrue:[extent := (extent x max: width) @ height].
	(properties hResizing == #shrinkWrap and:[properties rubberBandCells or:[vFill not]])
		ifTrue:[extent := width @ (extent y max: height)].

	posX := newBounds left.
	posY := newBounds top.

	"Compute extra horizontal space"
	extra := extent x - width.
	extra < 0 ifTrue:[extra := 0].
	extra > 0 ifTrue:[
		vFill ifTrue:[
			width := extent x.
		] ifFalse:[
			centering := properties wrapCentering.
			centering == #bottomRight ifTrue:[posX := posX + extra].
			centering == #center ifTrue:[posX := posX + (extra // 2)]
		].
	].


	"Compute extra vertical space"
	extra := extent y - height.
	extra < 0 ifTrue:[extra := 0].
	extraPerCell := 0.
	extra > 0 ifTrue:[
		sum isZero ifTrue:["extra space but no #spaceFillers"
			centering := properties listCentering.
			centering == #bottomRight ifTrue:[posY := posY + extra].
			centering == #center ifTrue:[posY := posY + (extra // 2)].
		] ifFalse:[extraPerCell := extra asFloat / sum asFloat].
	].

	n := 0.
	extra := last := 0.
	cell := first.
	[cell == nil] whileFalse:[
		n := n + 1.
		height := cell cellSize.
		(extraPerCell > 0 and:[cell vSpaceFill]) ifTrue:[
			extra := (last := extra) + (extraPerCell * cell extraSpace).
			amount := extra truncated - last truncated.
			height := height + amount.
		].
		cell target layoutInBounds: (posX @ posY extent: width @ height).
		posY := posY + height + inset.
		cell := cell nextCell.
	].
]

{ #category : #nil }
TableLayout >> minExtentHorizontal: aMorph [
	"Return the minimal size aMorph's children would require given the new bounds"
	| inset minX minY maxX maxY n size width height |
	size := properties minCellSize asPoint. minX := size x. minY := size y.
	size := properties maxCellSize asPoint. maxX := size x. maxY := size y.
	inset := properties cellInset asPoint.
	n := 0.
	width := height := 0.
	aMorph submorphsDo:[:m| | sizeY sizeX |
		m disableTableLayout ifFalse:[
			n := n + 1.
			size := m minExtent. sizeX := size x. sizeY := size y.
			sizeX < minX
				ifTrue:[sizeX := minX]
				ifFalse:[sizeX > maxX ifTrue:[sizeX := maxX]].
			sizeY < minY
				ifTrue:[sizeY := minY]
				ifFalse:[sizeY > maxY ifTrue:[sizeY := maxY]].
			width := width + sizeX.
			sizeY > height ifTrue:[height := sizeY].
		].
	].
	n > 1 ifTrue:[width := width + (n-1 * inset x)].
	^minExtentCache := width @ height
]

{ #category : #layout }
TableLayout >> minExtentOf: aMorph in: box [

	"Return the minimal size aMorph's children would require given the new bounds"

	| cells arrangement horizontal newBounds minX minY dir |

	minExtentCache ifNotNil: [ ^ minExtentCache ].
	aMorph hasSubmorphs
		ifFalse: [ ^ 0 @ 0 ].
	properties := aMorph assureTableProperties.
	( properties wrapDirection == #none and: [ properties cellSpacing == #none ] )
		ifTrue: [ "Get into the fast lane"
			dir := properties listDirection.
			( dir == #leftToRight or: [ dir == #rightToLeft ] )
				ifTrue: [ ^ self minExtentHorizontal: aMorph ].
			( dir == #topToBottom or: [ dir == #bottomToTop ] )
				ifTrue: [ ^ self minExtentVertical: aMorph ]
			].
	newBounds := box origin asIntegerPoint corner: box corner asIntegerPoint.
	horizontal := ( properties listDirection == #topToBottom
		or: [ properties listDirection == #bottomToTop ] ) not.	"Step 1: Compute the minimum extent for all the children of aMorph"
	cells := self
		computeCellSizes: aMorph
		in: ( 0 @ 0 corner: newBounds extent )
		horizontal: horizontal.	"Step 2: Compute the arrangement of the cells for each row and column"
	arrangement := self
		computeCellArrangement: cells
		in: newBounds
		horizontal: horizontal
		target: aMorph.	"Step 3: Extract the minimum size out of the arrangement"
	minX := minY := 0.
	arrangement
		do: [ :cell | 
			minX := minX max: cell cellSize x + cell extraSpace x.
			minY := minY + cell cellSize y + cell extraSpace y
			].
	minExtentCache := horizontal
		ifTrue: [ minX @ minY ]
		ifFalse: [ minY @ minX ].
	^ minExtentCache
]

{ #category : #nil }
TableLayout >> minExtentVertical: aMorph [
	"Return the minimal size aMorph's children would require given the new bounds"
	| inset minX minY maxX maxY n size width height |
	size := properties minCellSize asPoint. minX := size x. minY := size y.
	size := properties maxCellSize asPoint. maxX := size x. maxY := size y.
	inset := properties cellInset asPoint.
	n := 0.
	width := height := 0.
	aMorph submorphsDo:[:m| | sizeX sizeY |
		m disableTableLayout ifFalse:[
			n := n + 1.
			size := m minExtent. sizeX := size x. sizeY := size y.
			sizeX < minX
				ifTrue:[sizeX := minX]
				ifFalse:[sizeX > maxX ifTrue:[sizeX := maxX]].
			sizeY < minY
				ifTrue:[sizeY := minY]
				ifFalse:[sizeY > maxY ifTrue:[sizeY := maxY]].
			height := height + sizeY.
			sizeX > width ifTrue:[width := sizeX].
		].
	].
	n > 1 ifTrue:[height := height + (n-1 * inset y)].
	^minExtentCache := width @ height
]

{ #category : #layout }
TableLayout >> placeCells: arrangement in: newBounds horizontal: aBool target: aMorph [ 
	"Place the morphs within the cells accordingly"

	| xDir yDir anchor yDist place cell xDist cellRect corner inset |
	inset := properties cellInset.
	(inset isNumber and: [inset isZero]) ifTrue: [inset := nil].
	aBool 
		ifTrue: 
			["horizontal layout"

			properties listDirection == #rightToLeft 
				ifTrue: 
					[xDir := -1 @ 0.
					properties wrapDirection == #bottomToTop 
						ifTrue: 
							[yDir := 0 @ -1.
							anchor := newBounds bottomRight]
						ifFalse: 
							[yDir := 0 @ 1.
							anchor := newBounds topRight]]
				ifFalse: 
					[xDir := 1 @ 0.
					properties wrapDirection == #bottomToTop 
						ifTrue: 
							[yDir := 0 @ -1.
							anchor := newBounds bottomLeft]
						ifFalse: 
							[yDir := 0 @ 1.
							anchor := newBounds topLeft]]]
		ifFalse: 
			["vertical layout"

			properties listDirection == #bottomToTop 
				ifTrue: 
					[xDir := 0 @ -1.
					properties wrapDirection == #rightToLeft 
						ifTrue: 
							[yDir := -1 @ 0.
							anchor := newBounds bottomRight]
						ifFalse: 
							[yDir := 1 @ 0.
							anchor := newBounds bottomLeft]]
				ifFalse: 
					[xDir := 0 @ 1.
					anchor := properties wrapDirection == #rightToLeft 
								ifTrue: 
									[yDir := -1 @ 0.
									newBounds topRight]
								ifFalse: 
									[yDir := 1 @ 0.
									newBounds topLeft]]].
	1 to: arrangement size
		do: 
			[:i | 
			cell := arrangement at: i.
			cell extraSpace ifNotNil: [anchor := anchor + (cell extraSpace y * yDir)].
			yDist := cell cellSize y * yDir.	"secondary advance direction"
			place := anchor.
			cell := cell nextCell.
			[cell isNil] whileFalse: 
					[cell extraSpace ifNotNil: [place := place + (cell extraSpace x * xDir)].
					xDist := cell cellSize x * xDir.	"primary advance direction"
					corner := place + xDist + yDist.
					cellRect := Rectangle origin: (place min: corner)
								corner: (place max: corner).
					inset ifNotNil: [cellRect := cellRect insetBy: inset].
					cell target layoutInBounds: cellRect.
					place := place + xDist.
					cell := cell nextCell].
			anchor := anchor + yDist]
]
