*Morphic-Base
changeMaxCellSize: evt
	| handle |
	handle := HandleMorph new forEachPointDo:[:newPoint |
		self maxCellSize: (newPoint - evt cursorPoint) asIntegerPoint].
	evt hand attachMorph: handle.
	handle startStepping.
