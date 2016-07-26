adding
addAllAncestorsOf: aVersionInfo to: aSet
	(aSet includes: aVersionInfo) ifTrue: [^ self].
	aSet add: aVersionInfo.
	(self knownAncestorsOf: aVersionInfo) do:
		[:ea |
		self addAllAncestorsOf: ea to: aSet]