running
check: aSelector forDeprecationIn: aGlobalName

	^ (Smalltalk globals at: aGlobalName ifAbsent: [ ^ false ]) class
		classAndMethodFor: aSelector
		do: [ :class :method | method isDeprecated ]
		ifAbsent: [ false ]