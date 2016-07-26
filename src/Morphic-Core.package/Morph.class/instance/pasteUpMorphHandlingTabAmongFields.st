Morphic-Base-Worlds
pasteUpMorphHandlingTabAmongFields
	"Answer the nearest PasteUpMorph in my owner chain that has the tabAmongFields property, or nil if none"

	| aPasteUp |
	aPasteUp := self owner.
	[aPasteUp notNil] whileTrue:
		[aPasteUp tabAmongFields ifTrue:
			[^ aPasteUp].
		aPasteUp := aPasteUp owner].
	^ nil