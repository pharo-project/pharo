morph creation
newSliderIn: aThemedMorph for: aModel getValue: getSel setValue: setSel min: min max: max quantum: quantum getEnabled: enabledSel help: helpText
	"Answer a slider. Resizing set for horizontal usage by default."

	^(PluggableSliderMorph
			on: aModel
			getValue: getSel
			setValue: setSel
			min: min
			max: max
			quantum: quantum)
		getEnabledSelector: enabledSel;
		hResizing: #spaceFill;
		vResizing: #shrinkWrap;
		setBalloonText: helpText;
		minHeight: 16