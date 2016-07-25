testing
testFixedAccess
	| cache data keys |
	(cache := self newCache)
		maximumWeight: 4.
	data := (1 to: 16) collect: #asWords.
	data do: [ :each |
		cache at: each ifAbsentPut: [ each ] ].
	cache validateInvariantWith: self.
	self assert: cache size equals: 4.
	(#(14 15) collect: #asWords) do: [ :each |
		cache at: each ifAbsentPut: [ each ] ].
	self assert: cache size equals: 4.
	cache validateInvariantWith: self.	
	keys := data select: [ :each | cache includesKey: each ].
	keys do: [ :each | cache at: each ifAbsentPut: [ self fail ] ].
	self assert: cache hits >= 4.
	cache validateInvariantWith: self