instance-creation
newScrollbarMorph
	"Answer a new scrollbar morph."

	^ RubScrollBar new
		model: self;
		setValueSelector: #vScroll:;
		vResizing: #spaceFill;
		width: self theme scrollbarThickness