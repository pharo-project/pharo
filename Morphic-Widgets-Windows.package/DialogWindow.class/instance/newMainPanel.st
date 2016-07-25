actions
newMainPanel
	"Answer a new main panel."

	^self newDialogPanel
		addMorphBack: self newContentMorph;
		addMorphBack: self newButtonRow;
		yourself