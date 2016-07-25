construction
addSpace: sizePointOrNumber 
	"Add a new space of the given size to the receiver."
	| space |
	space := Morph new.
	space extent: sizePointOrNumber asPoint.
	space color: Color transparent.
	space borderWidth: 0.
	self addMorphBack: space