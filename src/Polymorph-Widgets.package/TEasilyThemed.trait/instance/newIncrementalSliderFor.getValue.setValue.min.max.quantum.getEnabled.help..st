controls
newIncrementalSliderFor: aModel getValue: getSel setValue: setSel min: min max: max quantum: quantum getEnabled: enabledSel help: helpText
	"Answer an inremental slider with the given selectors."

	^self theme
		newIncrementalSliderIn: self
		for: aModel
		getValue: getSel
		setValue: setSel
		min: min
		max: max
		quantum: quantum
		getEnabled: enabledSel
		help: helpText