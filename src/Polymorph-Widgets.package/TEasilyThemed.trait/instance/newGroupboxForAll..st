controls
newGroupboxForAll: controls
	"Answer a plain groupbox with the given controls."

	^self theme
		newGroupboxIn: self
		forAll: controls