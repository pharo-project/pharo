label-styles
windowLabelForText: aTextOrString
	"Answer the window label to use for the given text."
	^ LabelMorph new
		contents: aTextOrString;
		font: StandardFonts windowTitleFont emphasis: 0