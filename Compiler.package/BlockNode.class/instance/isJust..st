testing
isJust: node

	returns ifTrue: [^false].
	^statements size = 1 and: [statements first == node]