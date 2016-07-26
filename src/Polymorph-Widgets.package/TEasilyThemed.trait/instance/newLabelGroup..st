controls
newLabelGroup: labelsAndControls
	"Answer a morph laid out with a column of labels and a column of associated controls.
	Controls having a vResizing value of #spaceFill will
	cause their row to use #spaceFill also, otherwise #shrinkWrap."

	^self theme
		newLabelGroupIn: self
		for: labelsAndControls