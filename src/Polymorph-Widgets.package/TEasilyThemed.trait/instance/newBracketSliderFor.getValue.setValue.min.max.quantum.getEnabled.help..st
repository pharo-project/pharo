controls
newBracketSliderFor: aModel getValue: getSel setValue: setSel min: minValue max: maxValue quantum: quantum getEnabled: enabledSel help: helpText
	"Answer a bracket slider with the given selectors."

	^self theme
		newBracketSliderIn: self
		for: aModel
		getValue: getSel
		setValue: setSel
		min: minValue
		max: maxValue
		quantum: quantum
		getEnabled: enabledSel
		help: helpText