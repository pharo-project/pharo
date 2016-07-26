accessing
allSubclasses
	| allSubclasses index |
	index := 1.
	allSubclasses := OrderedCollection withAll: self subclasses.
	[index <= allSubclasses size] whileTrue: 
			[allSubclasses addAll: (allSubclasses at: index) subclasses.
			index := index + 1].
	^allSubclasses