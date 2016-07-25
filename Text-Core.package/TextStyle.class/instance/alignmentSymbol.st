accessing
alignmentSymbol
	"Answer the symbol for the current setting of the alignment."
	alignment = LeftFlush ifTrue:[^#leftFlush].
	alignment = Centered ifTrue:[^#centered].
	alignment = RightFlush ifTrue:[^#rightFlush].
	alignment = Justified ifTrue:[^#justified].
	^#leftFlush