privileged
enumerablePartPredicateIgnoringCase: aBoolean

	| enumeration |
	enumeration := self optimalSetIgnoringCase: aBoolean.
	^negated
		ifTrue: [[:char | (enumeration includes: char) not]]
		ifFalse: [[:char | enumeration includes: char]]