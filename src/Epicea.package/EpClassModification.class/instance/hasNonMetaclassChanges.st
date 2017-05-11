testing
hasNonMetaclassChanges

	^ oldClass classVarNames ~= newClass classVarNames
		or: [ oldClass instVarNames ~= newClass instVarNames
				or: [ oldClass sharedPoolNames ~= newClass sharedPoolNames
						or: [ oldClass superclassName ~= newClass superclassName ] ] ]