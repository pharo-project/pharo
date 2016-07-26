instance creation
newContentMorph
	"Answer a new content morph"

	^Morph new
		color: Color transparent;
		changeTableLayout;
		borderStyle: (BorderStyle inset width: 1);
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		addMorph: self newHatchMorph;
		yourself