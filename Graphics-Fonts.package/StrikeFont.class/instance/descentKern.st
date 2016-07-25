accessing
descentKern
	"Return the kern delta for descenders."
	(emphasis noMask: 2) ifTrue: [^ 0].
	^ (self height-1-self ascent+4)//4 max: 0  "See makeItalicGlyphs"

