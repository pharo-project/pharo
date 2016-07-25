testing
isAliasSelector
	"Return true if the selector is an alias (if it is different
	from the original selector) or already an aliased method
	in the original location (recursively search the compositions).
	Return false, if not or if we have a conflict."
	
	| locatedMethod |
	^self size = 1 and: [
		locatedMethod := self locatedMethods anyOne.
		(locatedMethod selector ~= self selector) or: [
			locatedMethod methodClass isAliasSelector: self selector]]