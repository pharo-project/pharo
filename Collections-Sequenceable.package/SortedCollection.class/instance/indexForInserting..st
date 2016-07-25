private
indexForInserting: newObject

	| index low high |
	low := firstIndex.
	high := lastIndex.
	sortBlock 
		ifNil: [[index := high + low // 2.  low > high]
			whileFalse: 
				[((array at: index) <= newObject)
					ifTrue: [low := index + 1]
					ifFalse: [high := index - 1]]]
		ifNotNil: [[index := high + low // 2.  low > high]
			whileFalse: 
				[(sortBlock value: (array at: index) value: newObject)
					ifTrue: [low := index + 1]
					ifFalse: [high := index - 1]]].
	^low