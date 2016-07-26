operations
newSpacer
	"Answer a spacer to place the controls to the right."

	^Morph new
		borderWidth: 0;
		color: Color transparent;
		extent: 0@0;
		minWidth: 0;
		hResizing: #spaceFill