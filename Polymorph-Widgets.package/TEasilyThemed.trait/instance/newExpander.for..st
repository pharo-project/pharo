controls
newExpander: aString for: aControl
	"Answer an expander with the given label and control."

	^self theme
		newExpanderIn: self
		label: aString
		forAll: {aControl}