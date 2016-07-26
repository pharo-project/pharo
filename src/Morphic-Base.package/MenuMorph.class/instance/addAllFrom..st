construction
addAllFrom: aMenuMorph
	"This is a fast add..."
	
	aMenuMorph submorphsDo: [ :m | self addMorphBack: m ]