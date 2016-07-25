initialization
initialize
	"Initialize the receiver."

	super initialize.
	self
		changeTableLayout;
		color: Color transparent;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		cellInset: 16;
		imageMorph: self newImageMorph;
		textMorph: self newTextMorph;
		addMorphBack: self imageMorph;
		addMorphBack: self textMorph