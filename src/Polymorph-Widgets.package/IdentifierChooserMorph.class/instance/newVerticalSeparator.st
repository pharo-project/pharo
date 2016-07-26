ui-building
newVerticalSeparator

	^ SeparatorMorph new
		fillStyle: Color transparent;
		borderStyle: (BorderStyle inset baseColor: Color gray; width: 1);
		extent: 1@1;
		vResizing: #spaceFill