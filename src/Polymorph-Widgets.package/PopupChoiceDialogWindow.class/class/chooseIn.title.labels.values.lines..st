instance creation
chooseIn: aThemedMorph title: title labels: labels values: values lines: lines
	"self chooseIn: World title: 'foo is the question'  labels: #('yes' 'no') values: #(true false) lines: #(1 2)"
	"self chooseIn: World title: 'foo is the question'  labels: #('yes' 'no') values: #(true false) lines: #()"
	| pd |
	pd := (self newWithTheme: aThemedMorph theme)
		title: (title isEmpty ifTrue: ['Choose' translated] ifFalse: [title asString]);
		labels: labels;
		lines: (lines ifNil: [#()]);
		model: values.
	^(aThemedMorph openModal: pd) choice