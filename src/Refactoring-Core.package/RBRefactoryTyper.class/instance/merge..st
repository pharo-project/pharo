equivalence classes
merge: aName 
	| set1 set2 |
	set1 := backpointers at: methodName ifAbsent: [nil].
	set2 := backpointers at: aName ifAbsent: [nil].
	(set1 isNil or: [set2 isNil or: [set1 == set2]]) ifTrue: [^self].
	set1 addAll: set2.
	set2 do: [:each | backpointers at: each put: set1]