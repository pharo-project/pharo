morph creation
newSeparatorIn: aThemedMorph
	"Answer a new horizontal separator."

	^SeparatorMorph new
		fillStyle: Color transparent;
		borderStyle: (BorderStyle inset baseColor: Color blue; width: 1);
		extent: 2@2;
		hResizing: #spaceFill