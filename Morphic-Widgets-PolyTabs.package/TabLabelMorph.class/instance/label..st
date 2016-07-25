accessing
label: aStringOrMorph
	"Change the label contents"

	|l|
	l := aStringOrMorph isMorph
		ifTrue: [aStringOrMorph]
		ifFalse: [(self theme buttonLabelForText: aStringOrMorph)
					vResizing: #shrinkWrap;
					hResizing: #shrinkWrap].
	self
		normalLabel: l;
		selectedLabel: l;
		selected: self isSelected