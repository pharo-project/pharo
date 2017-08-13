enquiries
changedSelectorsComparedTo: aTraitTransformation
	| selectors otherSelectors changedSelectors aliases otherAliases |
	selectors := self allSelectors asIdentitySet.
	otherSelectors := aTraitTransformation allSelectors asIdentitySet.
	changedSelectors := IdentitySet withAll: (
		(selectors difference: otherSelectors) union: (otherSelectors difference: selectors)).
	aliases := self allAliasesDict.
	otherAliases := aTraitTransformation allAliasesDict.
	aliases keysAndValuesDo: [:key :value |
		(value ~~ (otherAliases at: key ifAbsent: [nil])) ifTrue: [changedSelectors add: key]].
	otherAliases keysAndValuesDo: [:key :value |
		(value ~~ (aliases at: key ifAbsent: [nil])) ifTrue: [changedSelectors add: key]].
	^ changedSelectors.
