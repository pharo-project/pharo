controls
newGroupbox: aString forAll: controls
	"Answer a groupbox with the given label and controls."

	^self theme
		newGroupboxIn: self
		label: aString
		forAll: controls