border-styles-buttons
textEntryCornerStyleIn: aThemedMorph
	"Answer the corner style to use for text entry morphs."
	
	^aThemedMorph
		ifNil: [#square]
		ifNotNil: [:tm | tm preferredCornerStyle]