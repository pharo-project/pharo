accessing
label: aStringOrMorph selected: selectedStringOrMorph
	"Change the label contents"

	|l s|
	l := aStringOrMorph isMorph
		ifTrue: [aStringOrMorph]
		ifFalse: [(self theme buttonLabelForText: aStringOrMorph)
					vResizing: #shrinkWrap;
					hResizing: #shrinkWrap].
	s:= selectedStringOrMorph isMorph
		ifTrue: [selectedStringOrMorph]
		ifFalse: [(self theme buttonLabelForText: selectedStringOrMorph)
					vResizing: #shrinkWrap;
					hResizing: #shrinkWrap].
	self
		normalLabel: l;
		selectedLabel: s;
		selected: self isSelected