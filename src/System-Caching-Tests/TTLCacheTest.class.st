Class {
	#name : #TTLCacheTest,
	#superclass : #LRUCacheTest,
	#category : #'System-Caching-Tests'
}

{ #category : #testing }
TTLCacheTest class >> shouldInheritSelectors [
	^ true
]

{ #category : #accessing }
TTLCacheTest >> newCache [
	^ TTLCache new
]

{ #category : #testing }
TTLCacheTest >> testExpiring [
	| cache didComputeValue |
	cache := self newCache.
	cache timeToLive: 1 second.
	didComputeValue := false.
	cache at: #foo ifAbsentPut: [ didComputeValue := true. 100 ].
	self assert: didComputeValue.
	self assert: (cache includesKey: #foo).
	self assert: (cache at: #foo ifAbsentPut: [ self fail ]) equals: 100.
	1 second asDelay wait.
	didComputeValue := false.
	cache at: #foo ifAbsentPut: [ didComputeValue := true. 101 ].
	self assert: didComputeValue.
	self assert: (cache includesKey: #foo).
	self assert: (cache at: #foo ifAbsentPut: [ self fail ]) equals: 101.
	cache validateInvariantWith: self 
	
]

{ #category : #testing }
TTLCacheTest >> testRemoveStaleValues [
	| cache keys |
	cache := self newCache.
	cache timeToLive: 1 second.
	cache at: #foo ifAbsentPut: [ 100 ].
	cache at: #bar ifAbsentPut: [ 200 ].
	self deny: cache isEmpty.
	1 second asDelay wait.
	keys := cache removeStaleValues.
	self assertEmpty: cache.
	self assert: keys asArray sorted equals: #(bar foo).
	cache validateInvariantWith: self
]
