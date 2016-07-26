initialization
initialize
	"Initialize the receiver."

	super initialize.
	self
		roundedCorners: #(2 3 4);
		borderWidth: 0;
		changeTableLayout;
		cellPositioning: #topLeft;
		cellInset: 0 @ -1;
		reverseTableCells: true;
		labelMorph: self newLabelMorph;
		contentMorph: self newContentMorph;
		addMorphBack: self contentMorph;
		addMorphBack: self labelMorph