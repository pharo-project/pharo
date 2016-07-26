handles
positionIn: aBox horizontalPlacement: horiz verticalPlacement: vert
	| xCoord yCoord |

	horiz == #left
		ifTrue:	[xCoord := aBox left].
	horiz == #leftCenter
		ifTrue:	[xCoord := aBox left + (aBox width // 4)].
	horiz == #center
		ifTrue:	[xCoord := (aBox left + aBox right) // 2].
	horiz == #rightCenter
		ifTrue:	[xCoord := aBox left + ((3 * aBox width) // 4)].
	horiz == #right
		ifTrue:	[xCoord := aBox right].

	vert == #top
		ifTrue:	[yCoord := aBox top].
	vert == #topCenter
		ifTrue:	[yCoord := aBox top + (aBox height // 4)].
	vert == #center
		ifTrue:	[yCoord := (aBox top + aBox bottom) // 2].
	vert == #bottomCenter
		ifTrue:	[yCoord := aBox top + ((3 * aBox height) // 4)].
	vert == #bottom
		ifTrue:	[yCoord := aBox bottom].

	^ xCoord asInteger @ yCoord asInteger