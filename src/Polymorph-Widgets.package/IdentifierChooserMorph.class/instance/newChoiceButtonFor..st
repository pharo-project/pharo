ui-building
newChoiceButtonFor: index
	"Answer a new choice button."
	^ (self labels at: index) 
		ifNotNil: [:v | (ToggleMenuItemMorph new
			contents: v asText;
			target: self;
			selector: #choose:;
			arguments: {index})
			cornerStyle: #square;
			hResizing: #spaceFill]
		ifNil: [MenuLineMorph new]
		