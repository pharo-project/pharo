instance creation
newHatchMorph
	"Answer a new morph showing a grid hatch pattern."

	^Morph new
		color: Color transparent;
		changeProportionalLayout;
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		minWidth: 48;
		minHeight: 12;
		addMorph: (Morph new color: Color white)
		fullFrame: (0@0 corner: 0.3@1) asLayoutFrame ;
		addMorph: (Morph new fillStyle: (InfiniteForm with: self hatchForm))
		fullFrame: (0.3@0 corner: 0.7@1) asLayoutFrame;
		addMorph: self solidLabelMorph
		fullFrame: (0.7@0 corner: 1@1) asLayoutFrame;
		addMorph: self labelMorph
		fullFrame: (0@0 corner: 1@1) asLayoutFrame 