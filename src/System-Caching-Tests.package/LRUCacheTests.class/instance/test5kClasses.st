testing
test5kClasses
	| cache data |
	(cache := self newCache)
		maximumWeight: 1024.
	data := Object allSubclasses first: 5*1024.
	data do: [ :each |
		cache at: each name ifAbsentPut: [ each ] ].
	cache validateInvariantWith: self.
	self assert: cache size equals: 1024.
	data shuffled do: [ :each |
		cache at: each name ifAbsentPut: [ each ] ].
	self assert: cache size equals: 1024.
	cache validateInvariantWith: self.	
	data 
		select: [ :each | cache includesKey: each name ]
		thenDo: [ :each | cache at: each name ifAbsentPut: [ self fail ] ].
	self assert: cache hits >= 1024.
	cache validateInvariantWith: self