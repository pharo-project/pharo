operations
newSVColorMorph
	"Answer a new saturation/volume color morph."

	^SVColorSelectorMorph new
		extent: 152@152;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		when: #colorSelected send: #colorSelected: to: self