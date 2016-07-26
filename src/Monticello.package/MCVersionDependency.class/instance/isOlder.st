testing
isOlder
	"Answer true if I represent an older version of a package that is loaded."
	^ package hasWorkingCopy
		and: [self isFulfilled not
			and: [ self isFulfilledByAncestors
				and: [package workingCopy modified not]]]