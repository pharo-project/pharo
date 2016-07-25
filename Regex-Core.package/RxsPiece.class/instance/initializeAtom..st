initialize-release
initializeAtom: anAtom
	"This piece is exactly one occurrence of the specified RxsAtom."

	self initializeAtom: anAtom min: 1 max: 1