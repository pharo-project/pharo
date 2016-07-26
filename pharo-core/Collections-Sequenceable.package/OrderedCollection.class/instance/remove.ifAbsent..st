removing
remove: oldObject ifAbsent: absentBlock

	| index |
	index := firstIndex.
	[index <= lastIndex]
		whileTrue: 
			[oldObject = (array at: index)
				ifTrue: 
					[self removeIndex: index.
					^ oldObject]
				ifFalse: [index := index + 1]].
	^ absentBlock value