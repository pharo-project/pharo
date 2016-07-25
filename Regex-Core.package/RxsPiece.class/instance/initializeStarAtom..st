initialize-release
initializeStarAtom: anAtom
	"This piece is any number of occurrences of the atom."

	self initializeAtom: anAtom min: 0 max: nil