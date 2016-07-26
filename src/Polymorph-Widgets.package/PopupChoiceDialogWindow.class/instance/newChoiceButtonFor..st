as yet unclassified
newChoiceButtonFor: index
	"Answer a new choice button."

	^(ToggleMenuItemMorph new
		contents: (self labels at: index) asString;
		target: self;
		selector: #choose:;
		arguments: {index};
		getStateSelector: nil;
		enablementSelector: nil)
		cornerStyle: #square;
		hResizing: #spaceFill
		