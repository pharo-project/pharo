controls
newBracketSliderFor: aModel getValue: getSel setValue: setSel min: minValue max: maxValue quantum: quantum help: helpText
	"Answer a bracket slider with the given selectors."

	^self
		newBracketSliderFor: aModel
		getValue: getSel
		setValue: setSel
		min: minValue
		max: maxValue
		quantum: quantum 
		getEnabled: nil
		help: helpText