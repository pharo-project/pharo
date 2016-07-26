morph creation
newBalloonHelpIn: aThemedMorph contents: aTextStringOrMorph for: aMorph corner: cornerSymbol
	"Answer a new balloon help morph with the given text
	and positioning for aMorph."
	
	^SimpleBalloonMorph
		string: aTextStringOrMorph
		for: aMorph
		corner: cornerSymbol