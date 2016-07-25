instance-creation
newPrettyPrintCheckboxMorph
	"Answer a new checkbox for specifying whether to use
	pretty printing for the diff texts."

	^self
		newCheckboxFor: self
		getSelected: #prettyPrint
		setSelected: #prettyPrint:
		getEnabled: nil
		label: 'Pretty print' translated
		help: 'If selected, pretty print will be applied to any displayed method source (eliminates trivial formatting changes)' translated