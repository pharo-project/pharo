morph creation
newGroupboxIn: aThemedMorph for: control
	"Answer a plain groupbox."

	^(self newGroupboxIn: aThemedMorph)
		addMorph: control