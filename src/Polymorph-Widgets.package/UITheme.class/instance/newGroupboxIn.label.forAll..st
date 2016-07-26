morph creation
newGroupboxIn: aThemedMorph label: aString forAll: controls
	"Answer a groupbox with the given label and controls."

	^(self newGroupboxIn: aThemedMorph label: aString)
		addContentMorph: (self newColumnIn: aThemedMorph for: controls);
		yourself