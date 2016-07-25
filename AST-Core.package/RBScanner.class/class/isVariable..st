testing
isVariable: aString 
	| scanner token |
	scanner := self on: (ReadStream on: aString).
	token := scanner next.
	token isIdentifier ifFalse: [^false].
	(token start = 1 and: [token stop = aString size]) ifFalse: [^false].
	^(aString includes: $.) not