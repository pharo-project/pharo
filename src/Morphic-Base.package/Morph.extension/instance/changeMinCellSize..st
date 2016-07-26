*Morphic-Base
changeMinCellSize: evt
	| handle |
	handle := HandleMorph new forEachPointDo:[:newPoint |
		self minCellSize: (newPoint - evt cursorPoint) asIntegerPoint].
	evt hand attachMorph: handle.
	handle startStepping.
