controls
newSliderFor: aModel getValue: getSel setValue: setSel min: min max: max quantum: quantum getEnabled: enabledSel help: helpText
	"Answer a slider with the given selectors."

	^self theme
		newSliderIn: self
		for: aModel
		getValue: getSel
		setValue: setSel
		min: min
		max: max
		quantum: quantum
		getEnabled: enabledSel
		help: helpText