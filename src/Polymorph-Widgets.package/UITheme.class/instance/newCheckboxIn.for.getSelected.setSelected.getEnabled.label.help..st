morph creation
newCheckboxIn: aThemedMorph for: aModel getSelected: getSel setSelected: setSel getEnabled: enabledSel label: label help: helpText
	"Answer a checkbox with the given label ."

	| answer |
	answer := CheckboxMorph
			on: aModel selected: getSel changeSelected: setSel.
	answer
		getEnabledSelector: enabledSel;
		label: label;
		labelMorph: (self checkboxLabelFor: answer);
		font: self labelFont;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		setBalloonText: helpText.
	^answer