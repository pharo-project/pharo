accessing
ascentKern
	"Return the kern delta for ascenders."
	(emphasis noMask: 2) ifTrue: [^ 0].
	^ (self ascent-5+4)//4 max: 0  "See makeItalicGlyphs"

