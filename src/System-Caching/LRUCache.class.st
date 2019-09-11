"
I am LRUCache.
I am a Cache.

I am a limited cache that evicts the least recently used entries. My implementation is properly O(1).

Implementation Notes

The key/value pairs in the cache are held as Associations in a DoubleLinkedList, lruList, ordered from least to most recently used.

The keyIndex Dictionary maps from each key to the actual DoubleLink inside lruList holding the matching key/value pair.

New pairs are added at the end of the list.

In case of a hit, a pair gets promoted to the end of the list (most recently used).

In case of a full cache, the first pair of the list gets evicted (least recently used).

See #validateInvariantWith: where the relationship between the 2 datastructures is checked.
"
Class {
	#name : #LRUCache,
	#superclass : #AbstractCache,
	#instVars : [
		'lruList',
		'keyIndex'
	],
	#category : #'System-Caching'
}

{ #category : #private }
LRUCache >> addWeight: value [ 
	weight add: value.
	[ weight isBelowMaximum ]
		whileFalse: [ 
			self isEmpty
				ifTrue: [ self error: 'Weight of single value being added exceeds maximum' ]
				ifFalse: [ self evict ] ]
]

{ #category : #accessing }
LRUCache >> at: key ifAbsentPut: block [
	"If key is present in the cache, return the associated value.
	This is a hit and makes that key/value pair the most recently used.
	If key is absent, use block to compute a new value and cache it.
	Block can optionally take one argument, the key. 
	This is a miss and will create a new key/value pair entry.
	Furthermore this could result in the least recently used key/value pair 
	being removed when the specified maximum cache weight is exceeded."

	self critical: [ | association |
		association := keyIndex
			associationAt: key
			ifAbsent: [ | value |
				value := block cull: key.
				"Sadly we have to check the presence of key again 
				in case of the block execution already added the entry"
				keyIndex 
					associationAt: key 
					ifAbsent: [
						association := self newAssociationKey: key value: value.
						^ self handleMiss: association ] ].
		^ self handleHit: association ]
]

{ #category : #accessing }
LRUCache >> at: key put: value [
	"Populate me by storing value for key. Return value. 
	This is neither a hit nor a miss. Statistics remain unchanged.
	Overwrite if already present without promotion. 
	This could result in the least recently used key/value pair 
	being removed when the specified maximum cache weight is exceeded."
	
	self critical: [ | association link |
		association := keyIndex
			associationAt: key
			ifAbsent: [
				association := self newAssociationKey: key value: value.
				self addWeight: value.
				link := lruList addLast: association.
				keyIndex at: key put: link.
				^ value ].
		link := association value.
		weight remove: link value value.
		self addWeight: value.
		link value value: value.
		^ value ]
]

{ #category : #private }
LRUCache >> evict [
	| link value |
	link := lruList removeFirst.
	value := link value.
	weight remove: value value.
	keyIndex removeKey: value key
]

{ #category : #private }
LRUCache >> handleHit: association [
	| link |
	statistics addHit.
	link := association value.
	self promote: link.
	^ link value value
]

{ #category : #private }
LRUCache >> handleMiss: association [
	| link |
	statistics addMiss.
	self addWeight: association value.
	link := lruList addLast: association.
	keyIndex at: association key put: link.
	^ association value
]

{ #category : #testing }
LRUCache >> includesKey: key [
	"Return true when the receiver has a value cached for key."
	
	^ keyIndex includesKey: key
]

{ #category : #initialization }
LRUCache >> initialize [
	super initialize.
	keyIndex := Dictionary new.
	lruList := DoubleLinkedList new
]

{ #category : #testing }
LRUCache >> isEmpty [
	"Return true when the receiver contains no entries."
	
	^ keyIndex isEmpty
]

{ #category : #enumerating }
LRUCache >> keysAndValuesDo: block [
	"Execute block with each key and value present in me.
	This will be from least to most recently used."
	
	lruList do: [ :link |
		block 
			value: link key 
			value: link value ]
]

{ #category : #private }
LRUCache >> newAssociationKey: key value: value [
	^ Association key: key value: value
]

{ #category : #private }
LRUCache >> promote: link [
	lruList removeLink: link.
	lruList addLast: link
]

{ #category : #removing }
LRUCache >> removeAll [
	"Remove all key/value pairs that I currently hold,
	effectiley resetting me, but not my statistics."
	
	self critical: [
		lruList removeAll.
		keyIndex removeAll.
		weight reset ]
]

{ #category : #removing }
LRUCache >> removeKey: key ifAbsent: block [
	"If I currently cache key, remove the entry.
	Execute block when key is currently absent.
	Return the removed value."
	
	^ self critical: [
		(self includesKey: key)
			ifTrue: [ | link value |
				link := keyIndex removeKey: key.
				lruList removeLink: link.
				value := link value value.
				weight remove: value.
				value ]
			ifFalse: block ]
]

{ #category : #'accessing - statistics' }
LRUCache >> size [
	"Return the count of items currently present."
	
	^ keyIndex size
]

{ #category : #private }
LRUCache >> validateInvariantWith: assertable [
	"The keyIndex maps keys to double linked list nodes that hold 
	a key value association, whose key must match the index key"
	keyIndex keysAndValuesDo: [ :key :link |
		assertable assert: link value key = key ].
	"When iterating over all key value pairs, each key lookup up
	through the index should see the correct key and value in 
	the double linked list node"
	self keysAndValuesDo: [ :key :value |
		| link |
		link := keyIndex at: key.
		assertable assert: link value value = value.
		assertable assert: link value key = key ].
	"When iterating over all keys, each key can be present only once"
	Set new in: [ :keysSeen |
		self keysAndValuesDo: [ :key :value |
			assertable assert: (keysSeen includes: key) not.
			keysSeen add: key ] ]
]
