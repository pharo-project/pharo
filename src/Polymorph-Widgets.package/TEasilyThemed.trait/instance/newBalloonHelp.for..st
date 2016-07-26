controls
newBalloonHelp: aTextStringOrMorph for: aMorph
	"Answer a new balloon help with the given contents for aMorph
	at a given corner."

	^self theme
		newBalloonHelpIn: self
		contents: aTextStringOrMorph
		for: aMorph
		corner: #bottomLeft