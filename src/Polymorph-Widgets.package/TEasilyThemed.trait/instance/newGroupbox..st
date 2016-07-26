controls
newGroupbox: aString
	"Answer a groupbox with the given label."

	^self theme
		newGroupboxIn: self
		label: aString