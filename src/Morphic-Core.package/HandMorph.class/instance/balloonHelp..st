balloon help
balloonHelp: aBalloonMorph
	"Return the balloon morph associated with this hand"
	self balloonHelp ifNotNil:[:oldHelp |oldHelp delete].
	aBalloonMorph
		ifNil:[self removeProperty: #balloonHelpMorph]
		ifNotNil:[self setProperty: #balloonHelpMorph toValue: aBalloonMorph]