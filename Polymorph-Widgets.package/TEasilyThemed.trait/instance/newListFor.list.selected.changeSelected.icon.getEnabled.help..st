controls
newListFor: aModel list: listSelector selected: getSelector changeSelected: setSelector icon: iconSelector getEnabled: enabledSel help: helpText
	"Answer a list for the given model."

	^self theme
		newListIn: self
		for: aModel
		list: listSelector
		selected: getSelector
		changeSelected: setSelector
		icon: iconSelector
		getEnabled: enabledSel
		help: helpText