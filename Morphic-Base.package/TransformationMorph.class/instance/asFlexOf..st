initialization
asFlexOf: aMorph
	"Initialize me with position and bounds of aMorph,
	and with an offset that provides centered rotation."
	| pos |
	pos := aMorph position.
	self addMorph: aMorph.
	aMorph position: (aMorph extent // 2) negated.
	self position: pos.
	transform := transform withOffset: aMorph position - pos
