operations
newHSVColorMorph
	"Answer a new hue/saturation/volume color morph."

	^HSVColorSelectorMorph new
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		when: #colorSelected send: #colorSelected: to: self