"
I am Cache.
I am an abstract class.

I am a limited cache holding onto key/value pairs.

My primary interface is #at:ifAbsentPut: which takes two arguments: a key and a block. Either the key is found (cache hit) and its value is returned, or the key is not found (cache miss). If the latter case, block should compute a new value to cache. Because block takes the key as optional argument, you can specify a factory style argument as well. With an explicit factory specified, you can also use #at: to access me.

For each addition to the cache, a weight is computed by #computeWeight (a selector or block) and added to #totalWeight. When #totalWeight is no longer below #maximumWeight, the least recently used item of the cache is evicted (removed) to make room. 

The default #computeWeight returns 1 for each value, effectively counting the number of entries. The default #maximumWeight is 16.

I count hits and misses and can return my #hitRatio.

Optionally, but not by default, I can be configured so that it is safe to access me from different threads/processess during my important operations. See #beThreadSafe.
"
Class {
	#name : #AbstractCache,
	#superclass : #Object,
	#instVars : [
		'factory',
		'statistics',
		'weight',
		'access'
	],
	#category : #'System-Caching'
}

{ #category : #accessing }
AbstractCache >> addAll: keyedCollection [
	"Populate me with all key/value pairs from keyedCollection.
	Does not affect statistics."
	
	keyedCollection keysAndValuesDo: [ :key :value | 
		self at: key put: value ]
]

{ #category : #accessing }
AbstractCache >> at: key [
	"If key is present in the cache, return the associated value.
	This is a hit and makes that key/value pair the most recently used.
	If key is absent, use the factory to compute a new value and cache it.
	This is a miss and will create a new key/value pair entry.
	Furthermore this could result in the least recently used key/value pair 
	being removed when the specified maximum cache weight is exceeded.
	If there is no factory and the key is not present, signal a KeyNotFound exception."
	
	^ self 
		at: key 
		ifAbsentPut: (factory ifNil: [ [ :k | KeyNotFound signalFor: k in: self ] ])
]

{ #category : #accessing }
AbstractCache >> at: key ifAbsentPut: block [
	"If key is present in the cache, return the associated value.
	This is a hit and makes that key/value pair the most recently used.
	If key is absent, use block to compute a new value and cache it.
	Block can optionally take one argument, the key. 
	This is a miss and will create a new key/value pair entry.
	Furthermore this could result in the least recently used key/value pair 
	being removed when the specified maximum cache weight is exceeded."

	self subclassResponsibility 
]

{ #category : #accessing }
AbstractCache >> at: key put: value [
	"Populate me by storing value for key. Return value. 
	This is neither a hit nor a miss. Statistics remain unchanged.
	Overwrite if already present without promotion. 
	This could result in the least recently used key/value pair 
	being removed when the specified maximum cache weight is exceeded."
	
	self subclassResponsibility
]

{ #category : #initialize }
AbstractCache >> beThreadSafe [
	"Configure me so that I can be safely used from multiple threads/processes
	during important operations. Note that this slows down these operations."
	
	access := Monitor new
]

{ #category : #initialize }
AbstractCache >> computeWeight: valuable [
	"Set the way to compute the weight of each cached value.
	This can be either a Symbol or one argument block.
	When the total weight is no longer below the maximum weight,
	the least recently used key/value pair will be removed.
	The default way to compute the weight returns 1 for each value,
	effectively counting the number of cached values."
	
	weight compute: valuable
]

{ #category : #private }
AbstractCache >> critical: block [
	^ access
		ifNil: block
		ifNotNil: [ access critical: block ]
]

{ #category : #initialize }
AbstractCache >> factory: block [
	"Set the factory to compute values from keys to block.
	The factory will be evaluated for each key not present.
	Only my #at: message will use the factory."
	
	factory := block
]

{ #category : #'accessing - statistics' }
AbstractCache >> hitRatio [
	"Return the ratio of hits against total calls I received.
	This will be a number between 0 and 1.
	When I am empty, return 0."
	
	^ statistics hitRatio
]

{ #category : #'accessing - statistics' }
AbstractCache >> hits [
	"Return how many hits, requests for keys present I received."

	^ statistics hits
]

{ #category : #initialization }
AbstractCache >> initialize [
	super initialize.
	weight := CacheWeight new.
	statistics := CacheStatistics new
]

{ #category : #enumerating }
AbstractCache >> keysAndValuesDo: block [
	"Execute block with each key and value present in me.
	This will be from least to most recently used."
	
	self subclassResponsibility 
]

{ #category : #initialize }
AbstractCache >> maximumWeight: limit [
	"Set my maximum allowed total weight of all cached values to limit.
	If the total weight is no longer below limit,
	the least recently used key/value pair will be removed.
	The default maximum weight limit is 16."
	
	weight maximum: limit
]

{ #category : #'accessing - statistics' }
AbstractCache >> misses [
	"Return how many misses, requests for keys not present I received."
	
	^ statistics misses
]

{ #category : #printing }
AbstractCache >> printElementsOn: stream [
	stream nextPut: $#; print: self size.
	stream space; print: weight total; nextPut: $/; print: weight maximum.
	stream space; print: weight compute.
	factory ifNotNil: [ stream space; print: factory ].
	stream space; print: (self hitRatio * 100.0) rounded ; nextPut: $%
]

{ #category : #printing }
AbstractCache >> printOn: stream [
	super printOn: stream.
	stream nextPut: $(.
	self printElementsOn: stream.
	stream nextPut: $)
]

{ #category : #removing }
AbstractCache >> removeAll [
	"Remove all key/value pairs that I currently hold,
	effectiley resetting me, but not my statistics."

	self subclassResponsibility 
]

{ #category : #removing }
AbstractCache >> removeKey: key [
	"If I currently cache key, remove the entry.
	Signal a KeyNotFound when I currently do not cache key.
	Return the removed value."
	
	^ self 
		removeKey: key 
		ifAbsent: [ KeyNotFound signalFor: key in: self ]
]

{ #category : #removing }
AbstractCache >> removeKey: key ifAbsent: block [
	"If I currently cache key, remove the entry.
	Execute block when key is currently absent.
	Return the removed value."
	
	self subclassResponsibility 
]

{ #category : #'accessing - statistics' }
AbstractCache >> size [
	"Return the count of items currently present."
	
	self subclassResponsibility 
]

{ #category : #'accessing - statistics' }
AbstractCache >> totalWeight [
	"Return the total weight of all cached values currently present."
	
	^ weight total
]
