protocol
interactionState: aSymbol
	"Pass on to submorphs."

	self submorphsDo: [:m |
		(m respondsTo: #interactionState:) ifTrue: [
			m interactionState: aSymbol]]