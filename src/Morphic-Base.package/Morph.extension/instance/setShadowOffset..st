*Morphic-Base
setShadowOffset: evt
	| handle |
	handle := HandleMorph new forEachPointDo:
		[:newPoint | self shadowPoint: newPoint].
	evt hand attachMorph: handle.
	handle startStepping.
