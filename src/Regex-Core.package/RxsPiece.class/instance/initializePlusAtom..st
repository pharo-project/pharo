initialize-release
initializePlusAtom: anAtom
	"This piece is one or more occurrences of the specified RxsAtom."

	self initializeAtom: anAtom min: 1 max: nil