operations
showMorphs: aBoolean
	"Hide/Show the other morphs."

	self submorphs do: [:m |
		m == self titleMorph ifFalse: [
			m
				visible: aBoolean;
				disableTableLayout: aBoolean not]]