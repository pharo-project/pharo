*Morphic-Base
resizeMorph: evt
	| handle |
	handle := HandleMorph new forEachPointDo: [ :newPoint | self extent: newPoint - self bounds topLeft ].
	evt hand attachMorph: handle.
	handle startStepping