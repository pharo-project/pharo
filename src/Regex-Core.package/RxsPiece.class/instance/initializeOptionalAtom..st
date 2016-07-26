initialize-release
initializeOptionalAtom: anAtom
	"This piece is 0 or 1 occurrences of the specified RxsAtom."

	self initializeAtom: anAtom min: 0 max: 1