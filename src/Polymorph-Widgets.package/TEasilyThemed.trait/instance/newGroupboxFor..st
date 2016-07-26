controls
newGroupboxFor: control
	"Answer a plain groupbox with the given control."

	^self theme
		newGroupboxIn: self
		for: control