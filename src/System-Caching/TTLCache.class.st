"
I am TTLCache. 
I am an LRUCache.

I record a timestamp when I store a value for a key.

Upon a cache hit, I check if the timestamp of the stored value does not exceed the allowed time to live duration - if so, the value has become stale and I will retrieve the value again.

The default timeToLive is 1 hour.

Note that eviction, making room in a full cache, still happens according to the LRU algorithm from my superclass - stale entries to not get evicted automatically.

I can remove all my stale values in O(n), see #removeStaleValues.

Implementation Notes

I extend my superclass by storing TTLAssociations (which also hold a timestamp) instead of Associations in the DoubleLinkedList, lruList, ordered from least to most recently used.

In case of a hit, there is now an additional check to see if the value has become stale (exceeded its time to live). If so, the value is computed again.

Timestamps are implemented using Integer seconds (Time totalSeconds) for performance reasons.
"
Class {
	#name : #TTLCache,
	#superclass : #LRUCache,
	#instVars : [
		'timeToLive'
	],
	#category : #'System-Caching'
}

{ #category : #accessing }
TTLCache >> at: key ifAbsentPut: block [
	"Overwritten - see the comment of the method that I overwrite.
	In case of a hit, the cached value's timestamp is checked to see if its age
	does not exceed the allowed timeToLive. If so, the stale value is recomputed."
	
	self critical: [ | association |
		association := keyIndex 
			associationAt: key 
			ifAbsent: [ | value |
				value := block cull: key.
				"Sadly we have to check the presence of key again 
				in case of the block execution already added the entry"
				keyIndex 
					associationAt: key 
					ifAbsent: [ | newAssociation | 
						newAssociation := self newAssociationKey: key value: value.
						^ self handleMiss: newAssociation ] ].
		^ self handleHit: association ifStale: block ]
]

{ #category : #'private ' }
TTLCache >> handleHit: association ifStale: block [
	"In case of a hit, the cached value's timestamp is check to see if its age
	does not exceed the allowed timeToLive. If so, the stale value is recomputed."

	| link newValue |
	statistics addHit.
	link := association value.
	self promote: link.
	(self isStale: link value)
		ifTrue: [
			newValue := block cull: association key.
			link value value: newValue ].
	^ link value value
]

{ #category : #initialization }
TTLCache >> initialize [
	super initialize.
	self timeToLive: 1 hour
]

{ #category : #'private ' }
TTLCache >> isStale: association [
	"Delegate the decision to the association itself 
	with our time to live as default"
	
	^ association isStale: timeToLive
]

{ #category : #'private ' }
TTLCache >> newAssociationKey: key value: value [
	"Override this method in a subclass to customise expiration behavior"
	
	^ TTLAssociation key: key value: value
]

{ #category : #accessing }
TTLCache >> now [
	^ Time totalSeconds
]

{ #category : #printing }
TTLCache >> printElementsOn: stream [
	super printElementsOn: stream.
	stream space; print: timeToLive asDuration
]

{ #category : #removing }
TTLCache >> removeStaleValues [
	"Go over all cached values and remove those that are stale.
	Remove a collection of the keys that were removed."

	^ self critical: [
		| keysToRemove |
		keysToRemove := OrderedCollection new.
		lruList do: [ :association |
			 (self isStale: association) 
				ifTrue: [ keysToRemove add: association key ] ].
		keysToRemove do: [ :each | self removeKey: each ].
		keysToRemove ]
]

{ #category : #initialize }
TTLCache >> timeToLive: duration [
	"Set how old computed values can be before they become stale.
	When stale, a value will be computed again based on the block passed in.
	Specify either an Integer or a Duration, both will be converted to seconds."
	
	timeToLive := duration asDuration asSeconds
]
