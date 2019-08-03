"
I am NeoLRUCacheTests.

"
Class {
	#name : #LRUCacheTest,
	#superclass : #TestCase,
	#category : #'System-Caching-Tests'
}

{ #category : #accessing }
LRUCacheTest >> newCache [
	^ LRUCache new
]

{ #category : #testing }
LRUCacheTest >> test5kClasses [
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
]

{ #category : #testing }
LRUCacheTest >> test6k [
	| cache |
	(cache := self newCache)
		maximumWeight: 600.
	1 to: 6000 do: [ :each |
		cache at: each ifAbsentPut: [ each * 2 ] ].
	self assert: cache size equals: 600.
	self assert: (cache includesKey: 5401).
	self deny: (cache includesKey: 5400).
	cache validateInvariantWith: self
]

{ #category : #testing }
LRUCacheTest >> testAdditionInIfAbsentPut [
	| cache |
	cache := self newCache.
	cache at: #foo ifAbsentPut: [ 100 ].
	self assert: (cache at: #foo ifAbsentPut: [ self fail ]) equals: 100.
	"Add an entry for bar in the block computing the value to add"
	cache at: #bar ifAbsentPut: [ cache at: #bar ifAbsentPut: [ 200 ] ].
	self assert: (cache at: #bar ifAbsentPut: [ self fail ]) equals: 200.
	cache validateInvariantWith: self
]

{ #category : #testing }
LRUCacheTest >> testAt [
	| cache |
	cache := self newCache.
	self should: [ cache at: #foo ] raise: KeyNotFound.
	cache at: #foo ifAbsentPut: [ 100 ].
	self assert: (cache at: #foo) equals: 100.
	cache removeKey: #foo.
	self should: [ cache at: #foo ] raise: KeyNotFound
]

{ #category : #testing }
LRUCacheTest >> testCustomWeight [
	| cache |
	(cache := self newCache)
		computeWeight: #sizeInMemory;
		maximumWeight: 64.
	cache at: 1 ifAbsentPut: [ ByteArray new: 1 ].
	self assert: cache totalWeight equals: (ByteArray new: 1) sizeInMemory.
	2 to: 10 do: [ :each | 
		cache at: each ifAbsentPut: [ ByteArray new: each ] ].
	self assert: cache size equals: 3.
	self 
		assert: cache totalWeight 
		equals: ((8 to: 10) collect: [ :each | (ByteArray new: each) sizeInMemory ]) sum.
	self assert: (cache at: 10 ifAbsentPut: [ self fail ]) equals: (ByteArray new: 10).
	self deny: (cache includesKey: 1)
]

{ #category : #testing }
LRUCacheTest >> testEmpty [
	| emptyCache |
	emptyCache := self newCache.
	self assertEmpty: emptyCache.
	self assert: emptyCache size isZero.
	self assert: emptyCache hits isZero.
	self assert: emptyCache misses isZero.
	self assert: emptyCache hitRatio isZero.
	self assert: emptyCache totalWeight isZero.
	self deny: (emptyCache includesKey: #foo).
	emptyCache keysAndValuesDo: [ :key :value | self fail ]
]

{ #category : #testing }
LRUCacheTest >> testEnumeration [
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
]

{ #category : #testing }
LRUCacheTest >> testEviction [
	| cache |
	cache := self newCache.
	cache maximumWeight: 16.
	1 to: 20 do: [ :each |
		cache at: each asWords ifAbsentPut: [ each ] ].
	self assert: cache size equals: 16.
	self assert: cache totalWeight equals: 16.
	5 to: 20 do: [ :each |
		self assert: (cache includesKey: each asWords).
		self 
			assert: (cache at: each asWords ifAbsentPut: [ self fail ]) 
			equals: each ].
	cache validateInvariantWith: self
]

{ #category : #testing }
LRUCacheTest >> testFactory [
	| cache |
	cache := self newCache.
	cache maximumWeight: 5.
	cache factory: [ :key | key * 2 ].
	#( 1 2 3 4 1 5 6 7 8 1 ) do: [ :each |
		 cache at: each ].
	self assert: cache size equals: 5.
	self assert: cache hits equals: 2.
	cache validateInvariantWith: self
]

{ #category : #testing }
LRUCacheTest >> testFactoryStyle [
	| cache factory |
	cache := self newCache.
	cache maximumWeight: 5.
	factory := [ :key | key * 2 ].
	#( 1 2 3 4 1 5 6 7 8 1 ) do: [ :each |
		 cache at: each ifAbsentPut: factory ].
	self assert: cache size equals: 5.
	self assert: cache hits equals: 2.
	cache validateInvariantWith: self
]

{ #category : #testing }
LRUCacheTest >> testFibonacci [
	"After an idea by Jan Vrany.
	Recursively enter the cache and its access protection"
	
	| fibCache |
	fibCache := self newCache.
	fibCache 
		maximumWeight: 32;
		beThreadSafe;
		factory: [ :key |
			key < 2
				ifTrue: [ key ]
				ifFalse: [ (fibCache at: key - 1) + (fibCache at: key - 2) ] ].
	self assert: (fibCache at: 40) equals: 102334155
]

{ #category : #testing }
LRUCacheTest >> testFixedAccess [
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
]

{ #category : #testing }
LRUCacheTest >> testOne [
	| cache |
	cache := self newCache.
	cache at: #foo ifAbsentPut: [ 100 ].
	self assert: (cache includesKey: #foo).
	self deny: cache isEmpty.
	self assert: cache size equals: 1.
	self assert: cache totalWeight equals: 1.
	self assert: cache hits isZero.
	self assert: cache misses equals: 1.
	self assert: cache hitRatio isZero.
	self deny: (cache includesKey: #bar).
	cache validateInvariantWith: self
]

{ #category : #testing }
LRUCacheTest >> testOneHit [
	| cache value |
	cache := self newCache.
	cache at: #foo ifAbsentPut: [ 100 ].
	self assert: (cache includesKey: #foo).
	self assert: cache hits equals: 0.
	self assert: cache misses equals: 1.
	value := cache at: #foo ifAbsentPut: [ self fail ].
	self assert: value equals: 100.
	self assert: cache hits equals: 1.
	self assert: cache misses equals: 1.
	cache validateInvariantWith: self
	
]

{ #category : #testing }
LRUCacheTest >> testOneHitTwice [
	| cache value |
	cache := self newCache.
	cache at: #foo ifAbsentPut: [ 100 ].
	value := cache at: #foo ifAbsentPut: [ self fail ].
	self assert: value equals: 100.
	value := cache at: #foo ifAbsentPut: [ self fail ].
	self assert: value equals: 100.
	self assert: cache hitRatio equals: 2/3.
	cache validateInvariantWith: self
]

{ #category : #testing }
LRUCacheTest >> testPopulate [
	| cache data |
	cache := self newCache.
	cache at: #foo put: 1.
	self assert: cache hits isZero.
	self assert: cache misses isZero.
	self assert: cache size equals: 1.
	self assert: (cache at: #foo) equals: 1.
	cache at: #foo put: 2.
	self assert: cache size equals: 1.
	self assert: (cache at: #foo) equals: 2.
	cache := self newCache.
	data := { #x->1. #y->2. #z->3 } asDictionary.
	cache addAll: data.
	self assert: cache hits isZero.
	self assert: cache misses isZero.
	self assert: cache size equals: 3.
	data keysAndValuesDo: [ :key :value |
		self assert: (cache at: key) equals: value ]
]

{ #category : #testing }
LRUCacheTest >> testPrimeFactors [

	| cache data |
	cache := self newCache.
	cache maximumWeight: 512.
	cache factory: [ :key | key ].
	data := Array streamContents: [ :out |
		1 to: 4096 do: [ :each |
			each primeFactorsOn: out.
			out nextPut: each ] ].
	data := data collect: [ :each | each asWords ].
	data do: [ :each | cache at: each ].
	self assert: cache totalWeight equals: 512.
	self assert: cache hitRatio > (7/10).
	cache validateInvariantWith: self
]

{ #category : #testing }
LRUCacheTest >> testRandomAccess [
	| cache data random keys |
	(cache := self newCache)
		maximumWeight: 4.
	data := (1 to: 16) collect: #asWords.
	data do: [ :each |
		cache at: each ifAbsentPut: [ each ] ].
	cache validateInvariantWith: self.
	self assert: cache size equals: 4.
	random := data shuffled.
	random do: [ :each |
		cache at: each ifAbsentPut: [ each ] ].
	self assert: cache size equals: 4.
	cache validateInvariantWith: self.	
	keys := data select: [ :each | cache includesKey: each ].
	keys do: [ :each | cache at: each ifAbsentPut: [ self fail ] ].
	self assert: cache hits >= 4.
	cache validateInvariantWith: self
]

{ #category : #testing }
LRUCacheTest >> testRemoveAll [
	| cache |
	cache := self newCache.
	1 to: 10 do: [ :each | cache at: each ifAbsentPut: [ each ] ].
	cache removeAll.
	self assertEmpty: cache.
	self assert: cache size isZero.
	self assert: cache totalWeight isZero.
	self assert: cache misses equals: 10.
	self assert: cache hits isZero.
	self deny: (cache includesKey: 1).
	cache keysAndValuesDo: [ :key :value | self fail ].
	cache validateInvariantWith: self
]

{ #category : #testing }
LRUCacheTest >> testRemoveOne [
	| cache |
	cache := self newCache.
	cache at: #foo ifAbsentPut: [ 100 ].
	cache removeKey: #foo.
	self assertEmpty: cache.
	self assert: cache size isZero.
	self assert: cache totalWeight isZero.
	self deny: (cache includesKey: #foo).
	cache validateInvariantWith: self
]

{ #category : #testing }
LRUCacheTest >> testRemoveOneOfThree [
	| cache |
	cache := self newCache.
	cache at: #x ifAbsentPut: [ 100 ].
	cache at: #y ifAbsentPut: [ 200 ].
	cache at: #z ifAbsentPut: [ 300 ].
	cache removeKey: #y.
	self assert: cache size equals: 2.
	self assert: cache totalWeight equals: 2.
	self assert: (cache at: #x ifAbsentPut: [ self fail ]) equals: 100.
	self deny: (cache includesKey: #y).
	self assert: (cache at: #z ifAbsentPut: [ self fail ]) equals: 300.
	cache validateInvariantWith: self
]

{ #category : #testing }
LRUCacheTest >> testTen [
	| cache |
	cache := self newCache.
	1 to: 10 do: [ :each |
		cache at: each asWords ifAbsentPut: [ each ] ].
	self assert: cache size equals: 10.
	1 to: 10 do: [ :each |
		self assert: (cache includesKey: each asWords) ].
	1 to: 10 do: [ :each |
		self 
			assert: (cache at: each asWords ifAbsentPut: [ self fail ]) 
			equals: each ].
	self assert: cache hitRatio equals: 1/2.
	cache validateInvariantWith: self
]

{ #category : #testing }
LRUCacheTest >> testTenThreadSafe [
	| cache |
	cache := self newCache.
	cache beThreadSafe.
	1 to: 10 do: [ :each |
		cache at: each asWords ifAbsentPut: [ each ] ].
	self assert: cache size equals: 10.
	1 to: 10 do: [ :each |
		self 
			assert: (cache at: each asWords ifAbsentPut: [ self fail ]) 
			equals: each ].
	self assert: cache hitRatio equals: 1/2.
	cache validateInvariantWith: self
]

{ #category : #testing }
LRUCacheTest >> testThree [
	| cache |
	cache := self newCache.
	cache at: #x ifAbsentPut: [ 100 ].
	cache at: #y ifAbsentPut: [ 200 ].
	cache at: #z ifAbsentPut: [ 300 ].
	self assert: (cache at: #x ifAbsentPut: [ self fail ]) equals: 100.
	self assert: (cache at: #y ifAbsentPut: [ self fail ]) equals: 200.
	self assert: (cache at: #z ifAbsentPut: [ self fail ]) equals: 300.
	self assert: cache hitRatio equals: 3/6.
	self assert: (cache at: #z ifAbsentPut: [ self fail ]) equals: 300.
	self assert: (cache at: #y ifAbsentPut: [ self fail ]) equals: 200.
	self assert: (cache at: #x ifAbsentPut: [ self fail ]) equals: 100.
	self assert: cache hitRatio equals: 6/9.
	cache validateInvariantWith: self
]

{ #category : #testing }
LRUCacheTest >> testThreeHitSameOne [
	| cache |
	cache := self newCache.
	cache at: #x ifAbsentPut: [ 100 ].
	cache at: #y ifAbsentPut: [ 200 ].
	cache at: #z ifAbsentPut: [ 300 ].
	self assert: (cache at: #x ifAbsentPut: [ self fail ]) equals: 100.
	self assert: (cache at: #y ifAbsentPut: [ self fail ]) equals: 200.
	self assert: (cache at: #z ifAbsentPut: [ self fail ]) equals: 300.
	3 timesRepeat: [ 	
		self assert: (cache at: #x ifAbsentPut: [ self fail ]) equals: 100 ].
	self assert: cache hits equals: 6.
	cache validateInvariantWith: self
]

{ #category : #testing }
LRUCacheTest >> testTwo [
	| cache |
	cache := self newCache.
	cache at: #foo ifAbsentPut: [ 100 ].
	cache at: #bar ifAbsentPut: [ 200 ].
	self assert: (cache at: #foo ifAbsentPut: [ self fail ]) equals: 100.
	self assert: (cache at: #bar ifAbsentPut: [ self fail ]) equals: 200.
	self assert: cache hitRatio equals: 2/4.
	self assert: (cache at: #bar ifAbsentPut: [ self fail ]) equals: 200.
	self assert: (cache at: #foo ifAbsentPut: [ self fail ]) equals: 100.
	self assert: cache hitRatio equals: 4/6.
	cache validateInvariantWith: self
]
