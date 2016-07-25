dropping/grabbing
resetPotentialDropMorph
	"release the current drop morph candidate"
	potentialDropMorph ifNotNil: [
		potentialDropMorph resetHighlightForDrop.
		potentialDropMorph := nil]
