operations
addedMorph: aMorph
	"Notify the receiver that the given morph was just added."

	aMorph == self titleMorph ifFalse: [
		self titleMorph ifNotNil: [
			aMorph
				visible: self expanded;
				disableTableLayout: self expanded not]]