comparing
= anObject

	^ self == anObject
		ifTrue: [true]
		ifFalse: [anObject isInterval
			ifTrue: [start = anObject first
				and: [step = anObject increment
					and: [self last = anObject last]]]
			ifFalse: [super = anObject]]