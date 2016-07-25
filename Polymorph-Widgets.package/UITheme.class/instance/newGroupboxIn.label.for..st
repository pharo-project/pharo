morph creation
newGroupboxIn: aThemedMorph label: aString for: control
	"Answer a groupbox with the given label and control."

	^(self newGroupboxIn: aThemedMorph label: aString)
		addContentMorph: control;
		yourself