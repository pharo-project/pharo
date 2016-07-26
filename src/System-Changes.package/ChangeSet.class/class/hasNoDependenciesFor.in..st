filein/out
hasNoDependenciesFor: aTrait in: traits
	"Answer if the trait does not depend on a trait in the collection."

	^ traits allSatisfy: [ :another |
		aTrait == another or: [ "are the same"
			aTrait ~= another classSide and: [ "is not the classTrait of another"
				(aTrait traitComposition allTraits includes: another) not ] ] ]