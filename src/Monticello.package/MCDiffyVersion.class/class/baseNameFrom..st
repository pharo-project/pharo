name utilities
baseNameFrom: diffName
	| baseId verName |
	baseId := (diffName copyAfter: $() copyUpTo: $).
	baseId ifEmpty: [^baseId].
	(baseId beginsWith: '@')
		ifTrue: [^baseId copyAfter: $@].
	verName := self verNameFrom: diffName.
	^(baseId includes: $.)
		ifTrue: [(verName copyUpToLast: $-), '-', baseId]
		ifFalse: [(verName copyUpToLast: $.), '.', baseId]
