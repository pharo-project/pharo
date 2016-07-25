*Morphic-Base-Balloon
changeOriginIn: aMorph event: evt
	"Interactively change the origin of the receiver"
	| handle |
	handle := HandleMorph new forEachPointDo:[:pt|
		self origin: pt.
		aMorph changed].
	evt hand attachMorph: handle.
	handle startStepping.