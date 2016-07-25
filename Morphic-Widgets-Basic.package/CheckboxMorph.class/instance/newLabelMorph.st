private
newLabelMorph
	"Answer a new label morph"
	^	(LabelMorph contents: self label)
			on: #mouseDown send: #toggleSelected to: self; yourself