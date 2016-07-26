*CodeExport
nonTrivial 
	"Answer whether the receiver has any methods or instance variables."

	^ self instVarNames notEmpty or: [self hasMethods or: [self hasTraitComposition]]