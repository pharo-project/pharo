actions
newHSVAColorSelectorMorph
	"Answer a hsva color selector."

	^HSVAColorSelectorMorph new
		extent: (40@28) + 152;
		when: #selectedColor send: #colorSelected: to: self