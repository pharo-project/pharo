testing
isLocalAliasSelector
	"Return true if the selector is an alias (if it is different
	from the original selector). Return false, if not or if we
	have a conflict."
	
	^self size = 1 and: [
		(self locatedMethods anyOne selector ~= self selector)]