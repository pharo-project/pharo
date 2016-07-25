morph creation
newGroupboxIn: aThemedMorph
	"Answer a plain groupbox."

	^PlainGroupboxMorph new
		cornerStyle: aThemedMorph preferredCornerStyle;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		yourself