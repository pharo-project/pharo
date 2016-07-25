accessing-dynamic
ensureStashPersistency
	(self persistenceStrategies noneSatisfy: [ :each | each isStash ])
		ifTrue: [ self persistenceStrategies add: (GTPlayPageStashPersistence new page: self) ]