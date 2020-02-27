"
Like Set except a key of every element is used for hashing and searching instead of the element itself.  keyBlock gets the key of an element.
"
Class {
	#name : #OCKeyedSet,
	#superclass : #Set,
	#instVars : [
		'keyBlock'
	],
	#category : #'OpalCompiler-Core-Extras'
}

{ #category : #'instance creation' }
OCKeyedSet class >> keyBlock: oneArgBlock [
 	"Create a new KeySet whose way to access an element's key is by executing oneArgBlock on the element"
 
 	^ self new keyBlock: oneArgBlock
]

{ #category : #adding }
OCKeyedSet >> add: newObject [
	"Include newObject as one of the receiver's elements, but only if
	not already present. Answer newObject."

	| index |
	newObject ifNil: [self error: 'Sets cannot meaningfully contain nil as an element'].
	index := self findElementOrNil: (keyBlock value: newObject).
	(array at: index) ifNil: [self atNewIndex: index put: newObject].
	^ newObject
]

{ #category : #adding }
OCKeyedSet >> addAll: aCollection [ 
 	"Include all the elements of aCollection as the receiver's elements"
 
 	(aCollection respondsTo: #associationsDo:)
 		ifTrue: [aCollection associationsDo: [:ass | self add: ass]]
 		ifFalse: [aCollection do: [:each | self add: each]].
 	^ aCollection
]

{ #category : #accessing }
OCKeyedSet >> at: key [ 
 	"Answer the value associated with the key."
 
 	^ self at: key ifAbsent: [self errorKeyNotFound: key]
]

{ #category : #accessing }
OCKeyedSet >> at: key ifAbsent: aBlock [ 
 	"Answer the value associated with the key or, if key isn't found,
 	answer the result of evaluating aBlock."
 
 	^(array at: (self findElementOrNil: key)) 
		ifNil: [aBlock value]
		ifNotNil: [:o | o].
 	
]

{ #category : #accessing }
OCKeyedSet >> at: key ifAbsentPut: aBlock [ 
 	"Answer the value associated with the key or, if key isn't found,
 	add the result of evaluating aBlock to self"
 
 	^ self at: key ifAbsent: [self add: aBlock value]
]

{ #category : #accessing }
OCKeyedSet >> at: key ifPresent: aBlock [
 	"Lookup the given key in the receiver. If it is present, answer the value of evaluating the given block with the value associated with the key. Otherwise, answer nil."
 
 	| v |
 	v := self at: key ifAbsent: [^ nil].
 	^ aBlock value: v
 
]

{ #category : #copying }
OCKeyedSet >> collect: aBlock [ 
	"OCKeyedSet>>collect: must return an instance of Set and not an instance of his own class"

	| newSet |
	newSet := Set new: self size.
	array do: [:each | each ifNotNil: [newSet add: (aBlock value: each enclosedElement)]].
	^ newSet
]

{ #category : #copying }
OCKeyedSet >> copyEmpty [
	^super copyEmpty
		keyBlock: keyBlock copy
]

{ #category : #private }
OCKeyedSet >> errorKeyNotFound: aKey [

	KeyNotFound signalFor: aKey
]

{ #category : #private }
OCKeyedSet >> fixCollisionsFrom: start [
	"The element at start has been removed and replaced by nil.
	This method moves forward from there, relocating any entries
	that had been placed below due to collisions with this one"

	| element index |
	index := start.
	[ (element := array at: (index := index \\ array size + 1)) isNil ] whileFalse: [
		| newIndex |
		(newIndex := self findElementOrNil: (keyBlock value: element)) = index ifFalse: [
			array swap: index with: newIndex ] ]
]

{ #category : #testing }
OCKeyedSet >> includes: anObject [
	^ (array at: (self findElementOrNil: (keyBlock value: anObject))) notNil
]

{ #category : #testing }
OCKeyedSet >> includesKey: key [
	^ (array at: (self findElementOrNil: key)) notNil
]

{ #category : #initialization }
OCKeyedSet >> initialize: n [
 	super initialize: n.
 	keyBlock := [:element | element key].
 
]

{ #category : #testing }
OCKeyedSet >> isHealthy [
	"Opal developers plan to remove this class so for the moment we just return true/"

	^ true
]

{ #category : #initialize }
OCKeyedSet >> keyBlock: oneArgBlock [
 	"When evaluated return the key of the argument which will be an element of the set"
 
 	keyBlock := oneArgBlock
]

{ #category : #accessing }
OCKeyedSet >> keys [
 
 	| keys |
 	keys := Set new.
 	self keysDo: [:key | keys add: key].
 	^ keys
]

{ #category : #accessing }
OCKeyedSet >> keysDo: block [
 
 	self do: [:item | block value: (keyBlock value: item)]
]

{ #category : #adding }
OCKeyedSet >> like: anObject [
	"Answer an object in the receiver that is equal to anObject,
	nil if no such object is found. Relies heavily on hash properties"

	| index |

	^(index := self scanFor: (keyBlock value: anObject)) = 0
		ifFalse: [array at: index]
		ifTrue: [nil]
]

{ #category : #adding }
OCKeyedSet >> member: newObject [
 	"Include newObject as one of the receiver's elements, if already exists just return it"
 
 	| index |
 	newObject ifNil: [self error: 'Sets cannot meaningfully contain nil as an element'].
 	index := self findElementOrNil: (keyBlock value: newObject).
 	(array at: index) ifNotNil: [^ array at: index].
 	self atNewIndex: index put: newObject.
 	^ newObject
]

{ #category : #private }
OCKeyedSet >> noCheckAdd: anObject [
 	array at: (self findElementOrNil: (keyBlock value: anObject)) put: anObject.
 	tally := tally + 1
]

{ #category : #private }
OCKeyedSet >> rehash [
 	| newSelf |
 	newSelf := self species new: self size.
 	newSelf keyBlock: keyBlock.
 	self do: [:each | newSelf noCheckAdd: each].
 	array := newSelf array
]

{ #category : #removing }
OCKeyedSet >> remove: oldObject ifAbsent: aBlock [
 
 	| index |
 	index := self findElementOrNil: (keyBlock value: oldObject).
 	(array at: index) ifNil: [ ^ aBlock value ].
 	array at: index put: nil.
 	tally := tally - 1.
 	self fixCollisionsFrom: index.
 	^ oldObject
]

{ #category : #removing }
OCKeyedSet >> removeAll [
	"See super."
	
	| tmp |
	tmp := keyBlock.
	super removeAll.
	keyBlock := tmp
]

{ #category : #removing }
OCKeyedSet >> removeKey: key [
 
 	^ self removeKey: key ifAbsent: [self errorKeyNotFound: key]
]

{ #category : #removing }
OCKeyedSet >> removeKey: key ifAbsent: aBlock [
 
 	| index obj |
 	index := self findElementOrNil: key.
 	(obj := array at: index) ifNil: [ ^ aBlock value ].
 	array at: index put: nil.
 	tally := tally - 1.
 	self fixCollisionsFrom: index.
 	^ obj
]

{ #category : #private }
OCKeyedSet >> scanFor: anObject [
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."
	| element start finish |
	finish := array size.
	start := (anObject hash \\ finish) + 1.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element := array at: index) isNil or: [(keyBlock value: element) = anObject])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element := array at: index) isNil or: [(keyBlock value: element) = anObject])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"
]
