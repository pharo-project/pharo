testing
hasMetaclassChanges
	"FIX should compare metaclasses as a whole"

	^ oldClass theMetaClass instVarNames ~= newClass theMetaClass instVarNames