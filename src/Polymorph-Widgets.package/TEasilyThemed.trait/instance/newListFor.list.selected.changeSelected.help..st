controls
newListFor: aModel list: listSelector selected: getSelector changeSelected: setSelector help: helpText
	"Answer a list for the given model."

	^self
		newListFor: aModel
		list: listSelector
		selected: getSelector
		changeSelected: setSelector
		getEnabled: nil
		help: helpText