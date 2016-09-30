accessing
instVarIndexFor: aString ifAbsent: aBlockClosure
	| idx |
	idx := 1.
	slotScope do: [:each |
		each isVisible ifTrue: [
			each name = aString ifTrue: [^idx].
			idx := idx +1]].
	^aBlockClosure value