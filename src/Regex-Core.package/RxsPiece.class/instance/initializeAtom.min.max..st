initialize-release
initializeAtom: anAtom min: minOccurrences max: maxOccurrences
	"This piece is from <minOccurrences> to <maxOccurrences> 
	occurrences of the specified RxsAtom."

	atom := anAtom.
	min := minOccurrences.
	max := maxOccurrences