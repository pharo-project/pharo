*metacello-mc
resolveToAllPackagesIn: aVersionSpec visited: visited

	| packages |
	packages := Dictionary new.
	self resolveToAllPackagesIn: aVersionSpec into: packages visited: visited.
	^packages values asOrderedCollection