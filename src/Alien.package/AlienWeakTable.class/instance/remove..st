removing
remove: anObject
	1 to: lastUsedIndex do:
		[:i|
		(weakArray at: i) == anObject ifTrue:
			[accessProtect critical:
				[strongArray at: i put: nil.
				 firstUnusedIndex > i ifTrue:
					[firstUnusedIndex := i].
				 i = lastUsedIndex ifTrue:
					[lastUsedIndex := i - 1]].
			 ^anObject]].
	^anObject