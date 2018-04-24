"
I am a dictionary holding only weakly on my keys. This is a bit dangerous since at any time my keys can go away. Clients are responsible to register my instances by WeakArray such that the appropriate actions can be taken upon loss of any keys. As key may disappear at any time, my reported size may be greater than the number of keys encountered in iterations.

See WeakRegistry for an example of use.

Implementation notes:

I am a set of WeakAssociations. Each WeakAssociation can be in one of three states: a key is present, or the key has been garbage-collected, or the association is expired (meaning that the value has also been released). During finalization, associations with no key expire, but I still keep them to avoid rehashing the entire set. When adding a new entry, the new entry can either go into a slot that is nil, or one that has an expired association. I keep a count of expired associations and rehash when there are too many (currently, if they account for more than 25% of the space).
"
Class {
	#name : #WeakKeyDictionary,
	#superclass : #Dictionary,
	#instVars : [
		'expired',
		'finalizer'
	],
	#category : #'Collections-Weak-Dictionaries'
}

{ #category : #adding }
WeakKeyDictionary >> add: anAssociation [
	self at: anAssociation key put: anAssociation value.
	^ anAssociation
]

{ #category : #enumerating }
WeakKeyDictionary >> allAssociationsDo: aBlock [ 
	"Evaluate aBlock for each of the receiver's elements (key/value 
	associations). This includes associations with nil keys, except for
	the expired ones."

	super associationsDo: [:association | 
		association expired ifFalse:[aBlock value: association]].
]

{ #category : #enumerating }
WeakKeyDictionary >> associationsDo: aBlock [ 
	"Evaluate aBlock for each of the receiver's elements (key/value 
	associations)."

	

	super associationsDo: [:association | | key | 
		"Hold onto the key so it won't be collected while the block is evaluated."
		key := association key.
		key ifNotNil:[aBlock value: association]].
]

{ #category : #accessing }
WeakKeyDictionary >> at: key put: anObject [ 
	"Set the value at key to be anObject.  If key is not found, create a new
	entry for key and set is value to anObject. Answer anObject."
	| index element |
	key isNil ifTrue:[^anObject].
	index := self scanForEmpty: key.
	
	"There should always be room."
	index = 0 ifTrue: [ self error: 'No space left in dictionary' ].
	
	element := array at: index.
	element == nil
		ifTrue: [self atNewIndex: index put: (WeakKeyAssociation key: key value: anObject)]
		ifFalse: [
			element expired ifTrue: [ tally := tally + 1].
			element key: key.
			element value: anObject.
			self fullCheck.
		].
	^ anObject
]

{ #category : #private }
WeakKeyDictionary >> compare: object1 to: object2 [
	^object1 = object2
]

{ #category : #finalization }
WeakKeyDictionary >> expiredValuesDo: aBlock [
	"Clear all associations with nil keys"
	1 to: array size do:[:i | | assoc |
		assoc := array at: i.
		(assoc notNil and: [ assoc key isNil and: [assoc expired not ]])
		ifTrue: [ 
			aBlock value: assoc value.
			assoc expire.
			expired := expired + 1.
			tally := tally - 1.	]]
]

{ #category : #finalization }
WeakKeyDictionary >> finalizeValues [
	"Clear all associations with nil keys.
	Avoid using block closures, so it won't produce many garbage 
	(since pushing closure as argument requires creating its preinitialized copy)"
	1 to: array size do:[:i | | assoc |
		assoc := array at: i.
		assoc ifNotNil:[
			assoc expire ifTrue:[
				expired := expired + 1.
				tally := tally - 1.
			].
		].
	].

]

{ #category : #finalization }
WeakKeyDictionary >> finalizeValues: finiObjects [
	"Clear all associations with key == nil and value is in finiObjects."
	array do:[:assoc|
		(assoc notNil and:[finiObjects includes: assoc value]) ifTrue:[
			assoc expire.
			expired := expired + 1.
			tally := tally - 1.
		].
	].

]

{ #category : #private }
WeakKeyDictionary >> fixCollisionsFrom: oldIndex [
	"This is an internal method that should not be needed in WeakKeyDictionary"
	self shouldNotImplement.
]

{ #category : #private }
WeakKeyDictionary >> fullCheck [
	"Inlined super fullCheck, since the correct + X value for rehash check depends on its implementation"
	"Keep array at least 1/4 free for decent hash behavior"
	array size - tally < (array size // 4 max: 1)
		ifTrue: [self grow].
	"Make sure we never have so many expired entries that adding might fail"
	4 * expired + 4 > array size 
		ifTrue: [self rehash].
]

{ #category : #private }
WeakKeyDictionary >> grow [
	super grow.
	expired := 0.
]

{ #category : #initialization }
WeakKeyDictionary >> initialize: n [
	expired := 0.
	^super initialize: n
]

{ #category : #testing }
WeakKeyDictionary >> isHealthy [
	"Test that object hashes match their positions stored in the receiver,
	answer true if everything ok, false otherwise
	
	WeakKeyDictionary allInstances select: [:dict |
		dict isHealthy not ]
	Dictionary allSubInstances select: [:dict |
		dict isHealthy not ]
	"
	array withIndexDo: [:elem :i |
		(elem notNil and: [elem key notNil ])
				ifTrue:  [(self scanFor: elem key) == i ifFalse: [ ^ false ]
			]
	].
	^ true
]

{ #category : #private }
WeakKeyDictionary >> noCheckAdd: anAssociation [
	"Add anAssociation to the receiver. Discard expired associations. Put nil keys at the beginning."
	
	| key |
	
	key := anAssociation key. "Hold on to the key during this method"

	anAssociation expired ifFalse:[
		key ifNotNil:[
			super noCheckAdd: anAssociation
		] ifNil: [
			1 to: array size do:[:n|
				(array at: n) ifNil:[
					array at: n put: anAssociation. 
					tally := tally + 1.
					^self
				].
			].
			self error: 'could not find an empty slot.'
		].
	].
]

{ #category : #private }
WeakKeyDictionary >> noCheckAddForRehash: anAssociation [
	^self noCheckAdd: anAssociation
]

{ #category : #private }
WeakKeyDictionary >> noCheckNoGrowFillFrom: anArray [
	"Add the elements of anArray except nils and flag to me assuming that I don't contain any of them, they are unique and I have more free space than they require."

	tally := 0.
	1 to: anArray size do: [ :index |
		(anArray at: index) ifNotNil: [ :association |
			association key 
				ifNil: [ finalizer ifNotNil: [ finalizer value: association value ] ]
				ifNotNil: [ :key | "Don't let the key go away"
					array
						at: (self scanForEmptySlotFor: key)
						put: association.
					tally := tally + 1 ] ] ]
]

{ #category : #accessing }
WeakKeyDictionary >> overridingAt: key put: anObject [
	"Set the value at key to be anObject.  If key is not found, create a new
	entry for key and set is value to anObject. Answer anObject.
	May override an association with key == nil"
	
	| index element |
	key isNil ifTrue:[^anObject].
	index := self scanForKeyOrNil: key.
	
	"There should always be room."
	index = 0 ifTrue: [ self error: 'No space left in dictionary' ].
	
	element := array at: index.
	element == nil
		ifTrue: [self atNewIndex: index put: (WeakKeyAssociation key: key value: anObject)]
		ifFalse: [
			element expired ifTrue: [ tally := tally + 1].
			element key: key.
			element value: anObject.
			self fullCheck.
		].
	^ anObject
]

{ #category : #copying }
WeakKeyDictionary >> postCopy [
	"Must copy the associations, or later store will affect both the
original and the copy"

	array := array collect: [:assoc |
				assoc 
					ifNil: [nil]
					ifNotNil: [
						assoc expired 
							ifTrue: [ WeakKeyAssociation expired]
							ifFalse: [WeakKeyAssociation key: assoc key value: assoc value]]]
]

{ #category : #private }
WeakKeyDictionary >> rehash [
	"Rehash the receiver."
	super rehash.
	expired := 0.
]

{ #category : #removing }
WeakKeyDictionary >> removeKey: key ifAbsent: aBlock [
	"Remove key (and its associated value) from the receiver. If key is not in 
	the receiver, answer the result of evaluating aBlock. Otherwise, answer 
	the value externally named by key."

	| index result assoc |
	index := self findElementOrNil: key.
	assoc := array at: index.
	(assoc == nil or: [assoc key isNil]) ifTrue: [ ^ aBlock value ].
	assoc key: nil.
	result := assoc value.
	assoc expire.
	expired := expired + 1.
	tally := tally - 1.
	^result
]

{ #category : #private }
WeakKeyDictionary >> scanFor: anObject [
	"mvl 10/21/2007: The method has been copied to this location to indicate that whenever #scanFor: changes #scanForEmpty: must be changed in the receiver as well."
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. Expired associations are ignored, since callers take non-nil slots as a successful search. Subclasses should override startIndexFor: and compare:to: if they have a different semantics for matching elements."
	| element start finish |
	finish := array size.
	start := self startIndexFor: anObject.
	
	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element := array at: index) == nil or: [self compare: element key to: anObject])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element := array at: index) == nil or: [self compare: element key to: anObject])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"
]

{ #category : #private }
WeakKeyDictionary >> scanForEmpty: anObject [
	"Scan the key array for the first slot containing either a nil or an expired association (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found."
	| element start finish firstNil |
	finish := array size.
	start := self startIndexFor: anObject.
	firstNil := nil.
	
	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element := array at: index) == nil 
						or: [self compare: element key to: anObject])
			ifTrue: [
				"If we found no match, and have an expired slot, use that one."
				(element isNil and: [firstNil notNil]) ifTrue:[^firstNil].
				^ index 
			].
			"If this slot is expired, and we haven't seen an earlier expired slot, record it"
			(element notNil 
				and: [element expired 
						and: [firstNil isNil]]) ifTrue: [firstNil := index].
		].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element := array at: index) == nil 
					or: [self compare: element key to: anObject])
			ifTrue: [
				(element isNil and: [firstNil notNil]) ifTrue:[^firstNil].
				^ index 
			].
			(element notNil 
				and: [element expired 
						and: [firstNil isNil]]) ifTrue: [firstNil := index].
		].

	"If there is no empty slot, but an expired one, return it."	
	firstNil notNil ifTrue:[^firstNil].

	^ 0  "No match AND no empty slot"
]

{ #category : #private }
WeakKeyDictionary >> scanForKeyOrNil: anObject [
	"Same as scanFor: , but treats association with key == nil as empty slot"
	
	| element start finish |
	finish := array size.
	start := self startIndexFor: anObject.
	
	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element := array at: index) == nil or: [ element key isNil or: [self compare: element key to: anObject]])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element := array at: index) == nil or: [ element key isNil or: [self compare: element key to: anObject]])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"
]

{ #category : #accessing }
WeakKeyDictionary >> slowSize [
	"Careful! Answer the maximum amount
	of elements in the receiver, not the
	exact amount"

	| count |
	count := 0.
	1 to: array size do: [ :index |
		(array at: index) ifNotNil: [ :object |
			object expired ifFalse: [
				count := count + 1 ] ] ].
	^count
]

{ #category : #private }
WeakKeyDictionary >> startIndexFor: anObject [
	"Return the index at which the scan for anObject should start."
	
	^(anObject hash \\ array size) + 1
]

{ #category : #private }
WeakKeyDictionary >> valueAtNewKey: aKey put: anObject atIndex: index declareFrom: aDictionary [ 
	self shouldNotImplement.
]

{ #category : #enumerating }
WeakKeyDictionary >> valuesDo: aBlock [ 
	"See comments in Dictionary>>valuesDo:.  The code keeps the key so it's
	not collected during the evaluation of aBlock"

	tally = 0 ifTrue: [ ^self ].
	1 to: array size do:
		[ :eachIndex | 
			| eachAssociation eachKey |
			eachAssociation := array at: eachIndex.
			nil == eachAssociation ifFalse:
				[
					eachKey := eachAssociation key.
					aBlock value: eachAssociation value
				]
		]
]
