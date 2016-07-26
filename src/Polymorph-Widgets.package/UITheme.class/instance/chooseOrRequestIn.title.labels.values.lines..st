services
chooseOrRequestIn: aThemedMorph title: title labels: labels values: values lines: lines
	"Answer the result of a popup choice with the given title, labels, values and lines."

	| dialog choice |
	dialog := (PopupChoiceOrRequestDialogWindow newWithTheme: aThemedMorph theme)
		title: (title isEmpty ifTrue: ['Choose' translated] ifFalse: [title asString]);
		labels: labels;
		lines: (lines ifNil: [#()]);
		model: values.
	choice := (aThemedMorph openModal: dialog) choice.
	dialog cancelled ifTrue: [ ^nil ].
	choice 
		ifNotNil: [ ^choice ]
		ifNil: [ ^dialog filterValue ]