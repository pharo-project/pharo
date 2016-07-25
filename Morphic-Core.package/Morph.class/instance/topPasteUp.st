Morphic-Base-Worlds
topPasteUp
	"If the receiver is in a world, return that; otherwise return the outermost pasteup morph"
	^ self outermostMorphThat: [:m | m isKindOf: PasteUpMorph]