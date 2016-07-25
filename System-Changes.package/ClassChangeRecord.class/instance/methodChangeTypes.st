method changes
methodChangeTypes
	"Return an old-style dictionary of method change types."

	| dict |
	dict := IdentityDictionary new.
	methodChanges associationsDo:
		[:assn | | selector record |
		selector := assn key.
		record := assn value.
		dict at: selector put: record changeType].
	^ dict