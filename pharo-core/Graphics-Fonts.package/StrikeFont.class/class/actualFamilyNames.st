accessing
actualFamilyNames
	"Answer a sorted list of actual family names, without the Default aliases"

	^(self familyNames copyWithoutAll: TextStyle defaultFamilyNames) asOrderedCollection