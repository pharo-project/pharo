comparing
= anotherObject
	^self species == anotherObject species 
		and: [self value = anotherObject value 
				and: [self nextLink == anotherObject nextLink]]