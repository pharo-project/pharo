textconstants access
knownTextStylesWithoutDefault
	"Answer the names of the known text styles, sorted in alphabetical order without default"

	"TextStyle knownTextStylesWithoutDefault"
	| result |
	result := self knownTextStyles asOrderedCollection.
	^ result copyWithoutAll: self defaultFamilyNames

