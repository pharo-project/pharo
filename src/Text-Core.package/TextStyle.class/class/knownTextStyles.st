textconstants access
knownTextStyles
	"Answer the names of the known text styles, sorted in alphabetical order"

	"TextStyle knownTextStyles"
	^ (TextSharedInformation select: [:thang | thang isKindOf: TextStyle]) keys sort

