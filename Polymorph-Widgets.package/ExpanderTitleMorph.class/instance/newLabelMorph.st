instance creation
newLabelMorph
	"Answer a new label morph for the receiver."

	^LabelMorph new
		hResizing: #spaceFill;
		vResizing: #shrinkWrap