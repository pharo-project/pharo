morph creation
newGroupboxIn: aThemedMorph forAll: controls
	"Answer a plain groupbox."

	^self
		newGroupboxIn: aThemedMorph
		for: (self newColumnIn: aThemedMorph for: controls)