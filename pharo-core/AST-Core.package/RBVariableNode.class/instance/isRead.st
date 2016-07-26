testing
isRead
	^ self isWrite not and: [ self isUsed ]