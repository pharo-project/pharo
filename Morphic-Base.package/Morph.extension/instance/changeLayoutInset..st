*Morphic-Base
changeLayoutInset: evt
	| handle |
	handle := HandleMorph new forEachPointDo:[:newPoint |
		self layoutInset: (newPoint - evt cursorPoint) asIntegerPoint // 5].
	evt hand attachMorph: handle.
	handle startStepping.
