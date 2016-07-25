morph creation
newBracketSliderIn: aThemedMorph for: aModel getValue: getSel setValue: setSel min: minValue max: maxValue quantum: quantum getEnabled: enabledSel help: helpText
	"Answer a bracket slider."

	^(BracketSliderMorph
			on: aModel
			getValue: getSel
			setValue: setSel
			min: minValue
			max: maxValue
			quantum: quantum)
		getEnabledSelector: enabledSel;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		setBalloonText: helpText