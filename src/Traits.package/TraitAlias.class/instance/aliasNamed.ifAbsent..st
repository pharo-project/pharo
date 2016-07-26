enumeration
aliasNamed: aSymbol ifAbsent: aBlock
	^self aliases
		detect: [:association |  association key = aSymbol]
		ifNone: aBlock