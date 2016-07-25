layout
computeExtraSpacing: arrangement in: newBounds horizontal: aBool target: aMorph 
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