converting
asLocal
	
	^ (self offset = self class localOffset)
		ifTrue: [self]
		ifFalse: [self offset: self class localOffset]
