controls
newExpander: aString
	"Answer an expander with the given label."

	^self theme
		newExpanderIn: self
		label: aString
		forAll: #()