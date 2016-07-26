services
chooseIn: aThemedMorph title: title labels: labels values: values lines: lines
	"Answer the result of a popup choice with the given title, labels, values and lines."

	| pd |
	pd := (PopupChoiceDialogWindow newWithTheme: aThemedMorph theme)
		title: (title isEmpty ifTrue: ['Choose' translated] ifFalse: [title asString]);
		labels: labels;
		lines: (lines ifNil: [#()]);
		model: values.
	^(aThemedMorph openModal: pd) choice