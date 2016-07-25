initialization
initialize
	"Initialize the receiver."

	super initialize.
	self sliderMorph: self newSliderMorph.
	self
		changeTableLayout;
		listDirection: #leftToRight;
		cellInset: 0;
		borderWidth: 0;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		borderColor: Color transparent;
		addMorphBack: self newDecrementButton;
		addMorphBack: self sliderMorph;
		addMorphBack: self newIncrementButton