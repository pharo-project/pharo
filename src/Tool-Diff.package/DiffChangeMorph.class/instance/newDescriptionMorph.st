actions
newDescriptionMorph
	"Answer a new morph for the descriptions of the source and destination."

	^(self newColumn: {
		(self newRow: {self newLabel: 'Original'}) layoutInset: 1.
		(self newRow: {self newLabel: 'Changed'})
			layoutInset: 1;
			listCentering: #bottomRight})
		layoutInset: 0;
		cellInset: 2;
		"fillStyle: Color white;"
		borderStyle: (self theme listNormalBorderStyleFor: self)
		