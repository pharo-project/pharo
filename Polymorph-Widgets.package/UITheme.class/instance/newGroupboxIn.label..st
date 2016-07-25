morph creation
newGroupboxIn: aThemedMorph label: aString
	"Answer a groupbox with the given label."

	^GroupboxMorph new
		font: self labelFont;
		cornerStyle: aThemedMorph preferredCornerStyle;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		label: aString;
		yourself