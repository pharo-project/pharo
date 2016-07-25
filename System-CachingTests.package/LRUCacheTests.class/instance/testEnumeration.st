testing
testEnumeration
	| cache data keys values |
	cache := self newCache.
	data := Dictionary new.
	1 to: 10 do: [ :each | data at: each asWords put: each ].
	data keysAndValuesDo: [ :key :value |
		cache at: key ifAbsentPut: [ value ] ].
	keys := Array new writeStream.
	values := Array new writeStream.
	cache keysAndValuesDo: [ :key :value |
		self assert: (data at: key) equals: value.
		keys nextPut: key.
		values nextPut: value ].
	self assert: keys contents asSet equals: data keys asSet.
	self assert: values contents asSet equals: data values asSet.
	cache validateInvariantWith: self