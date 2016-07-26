controls
newGroupbox: aString for: control
	"Answer a groupbox with the given label and control."

	^self theme
		newGroupboxIn: self
		label: aString
		for: control