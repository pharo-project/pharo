morph creation
newScrollPaneIn: aThemedMorph for: aMorph
	"Answer a new GeneralScrollPane with given content morph."

	^GeneralScrollPane new
		scrollTarget: aMorph;
		yourself