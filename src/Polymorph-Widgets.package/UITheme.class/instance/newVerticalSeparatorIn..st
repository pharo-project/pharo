morph creation
newVerticalSeparatorIn: aThemedMorph
	"Answer a new vertical separator."

	^SeparatorMorph new
		fillStyle: Color transparent;
		borderStyle: (BorderStyle inset baseColor: Color blue; width: 1);
		extent: 2@2;
		vResizing: #spaceFill