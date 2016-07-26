privileged
optimalSetIgnoringCase: aBoolean
	"Assuming the client with search the `set' using #includes:,
	answer a collection with the contents of `set', of the class
	that will provide the fastest lookup. Strings are faster than
	Sets for short strings."

	| set |
	set := self enumerableSetIgnoringCase: aBoolean.
	^set size < 10
		ifTrue: [set asArray]
		ifFalse: [set]