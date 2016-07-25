initialization
initialize
	"Initialize the receiver."

	super initialize.
	self
		borderWidth: 0;
		changeTableLayout;
		labelMorph: self newLabelMorph;
		solidLabelMorph: self newLabelMorph;
		contentMorph: self newContentMorph;
		addMorphBack: self contentMorph