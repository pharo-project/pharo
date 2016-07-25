morph creation
newTabGroupIn: aThemedMorph for: labelsAndControls
	"Answer a tab group morph with the given tab labels and associated pages."

	^(TabGroupMorph new
		font: self labelFont;
		cornerStyle: (self tabGroupCornerStyleIn: aThemedMorph);
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		labelsAndPages: labelsAndControls)
		selectedPageIndex: (labelsAndControls isEmpty ifTrue: [0] ifFalse: [1])