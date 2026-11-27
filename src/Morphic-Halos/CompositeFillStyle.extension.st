Extension { #name : 'CompositeFillStyle' }

{ #category : '*Morphic-Halos' }
CompositeFillStyle >> changeOrientationIn: aMorph event: evt [
	"Interactively change the origin of the receiver"
	| handle |
	handle := HandleMorph new forEachPointDo:[:pt|
		self direction: pt - self origin.
		self normal: nil.
		aMorph changed].
	evt hand attachMorph: handle.
	handle startStepping
]

{ #category : '*Morphic-Halos' }
CompositeFillStyle >> changeOriginIn: aMorph event: evt [
	"Interactively change the origin of the receiver"
	| handle |
	handle := HandleMorph new forEachPointDo:[:pt|
		self origin: pt.
		aMorph changed].
	evt hand attachMorph: handle.
	handle startStepping
]
