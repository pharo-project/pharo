Class {
	#name : #GTSpotterIterator,
	#superclass : #Object,
	#instVars : [
		'context',
		'items'
	],
	#category : #'GT-Spotter-Iterators'
}

{ #category : #accessing }
GTSpotterIterator class >> gtIterator [
	^ self new
]

{ #category : #'mimic various enumerating' }
GTSpotterIterator >> allSatisfy: aBlock [
	self do: [ :each | (aBlock value: each) ifFalse: [ ^ false ] ].
	^ true
]

{ #category : #'mimic various enumerating' }
GTSpotterIterator >> anySatisfy: aBlock [
	self do: [ :each | (aBlock value: each) ifTrue: [ ^ true ] ].
	^ false
]

{ #category : #'mimic collection' }
GTSpotterIterator >> asArray [
	^ self items asArray
]

{ #category : #'mimic collection' }
GTSpotterIterator >> asOrderedCollection [
	^ self items copy " we must not expose the internal representation ! "
]

{ #category : #'mimic collection' }
GTSpotterIterator >> asSet [
	^ self items asSet
]

{ #category : #'mimic various enumerating' }
GTSpotterIterator >> collect: aBlock [
	^ self collect: aBlock as: self defaultItemsClass
]

{ #category : #'mimic various enumerating' }
GTSpotterIterator >> collect: aBlock as: aCollectionClass [
	| collection |
	collection := self newAs: aCollectionClass.
	self do: [ :each | 
		collection add: (aBlock value: each) ].
	^ collection
]

{ #category : #accessing }
GTSpotterIterator >> context [
	^ context
]

{ #category : #accessing }
GTSpotterIterator >> context: aContext [
	" we assume that if the context changes, so will the result of the iterator - so we reset the internal collection "
	context := aContext.
	self reset.
]

{ #category : #public }
GTSpotterIterator >> cull: aContext [
	self context: aContext
]

{ #category : #'accessing-defaults' }
GTSpotterIterator >> defaultItems [
	^ self defaultItemsClass new 
]

{ #category : #'accessing-defaults' }
GTSpotterIterator >> defaultItemsClass [
	^ OrderedCollection 
]

{ #category : #'mimic various enumerating' }
GTSpotterIterator >> detect: aBlock [
	^ self detect: aBlock ifNone: [ NotFound signalFor: aBlock ]
]

{ #category : #'mimic various enumerating' }
GTSpotterIterator >> detect: aBlock ifFound: aBlock2 ifNone: aBlock3 [
	self do: [ :each | aBlock value ifTrue: [ ^ aBlock2 value: each ] ].
	^ aBlock3 value
]

{ #category : #'mimic various enumerating' }
GTSpotterIterator >> detect: aBlock ifNone: aBlock2 [
	^ self detect: aBlock ifFound: [ :each | each ] ifNone: aBlock2
]

{ #category : #'mimic enumerating' }
GTSpotterIterator >> do: aBlock [
	" the iterator must keep an internal collection of items being iterated. This guarantees that the iteration is not done multiple times in case somebody sends multiple subsequent messages like #size, #items or #first:. This will slightly decrease performance while iterating "
	items 
		ifNotNil: [ 
			items do: aBlock ]
		ifNil: [ 
			items := self defaultItems.
			self itemDo: [ :each | 
				items add: each.
				aBlock value: each ] ]
]

{ #category : #'mimic collection' }
GTSpotterIterator >> first: anInteger [
	^ self items copyFrom: 1 to: (self size min: (anInteger max: 0))
]

{ #category : #public }
GTSpotterIterator >> gtIterator [
	^ self
]

{ #category : #testing }
GTSpotterIterator >> hasDynamicItems [
	^ true
]

{ #category : #'mimic collection' }
GTSpotterIterator >> isEmpty [
	^ self items isEmpty
]

{ #category : #'mimic collection' }
GTSpotterIterator >> isEmptyOrNil [
	^ self items isEmptyOrNil
]

{ #category : #private }
GTSpotterIterator >> itemDo: aBlock [
	self subclassResponsibility
]

{ #category : #private }
GTSpotterIterator >> items [
	^ items ifNil: [ 
		items := self defaultItems.
		self itemDo: [ :each | 
			items add: each ] ]
]

{ #category : #private }
GTSpotterIterator >> items: anObject [
	items := anObject
]

{ #category : #'mimic collection' }
GTSpotterIterator >> last: anInteger [
	^ self items last: (self size min: (anInteger max: 0))
]

{ #category : #private }
GTSpotterIterator >> newAs: aCollectionClass [
	^ items 
		ifNil: [ aCollectionClass new ] 
		ifNotNil: [ aCollectionClass new: self size ]
]

{ #category : #'mimic collection' }
GTSpotterIterator >> notEmpty [
	^ self isEmpty not
]

{ #category : #'mimic various enumerating' }
GTSpotterIterator >> reject: aBlock [
	^ self reject: aBlock as: self defaultItemsClass
]

{ #category : #'mimic various enumerating' }
GTSpotterIterator >> reject: aBlock as: aCollectionClass [
	| collection |
	collection := self newAs: aCollectionClass.
	self do: [ :each | 
		(aBlock value: each) 
			ifFalse: [ collection add: each ] ].
	^ collection
]

{ #category : #'mimic various enumerating' }
GTSpotterIterator >> reject: aBlock thenCollect: aBlock2 as: aCollectionClass [
	| collection |
	collection := self newAs: aCollectionClass.
	self reject: aBlock thenDo: [ :each | 
		collection add: (aBlock2 value: each) ].
	^ collection
]

{ #category : #'mimic various enumerating' }
GTSpotterIterator >> reject: aBlock thenDo: aBlock2 [
	self do: [ :each | 
		(aBlock value: each) 
			ifFalse: [ aBlock2 value: each ] ]
]

{ #category : #public }
GTSpotterIterator >> reset [
	items := nil.
]

{ #category : #'mimic various enumerating' }
GTSpotterIterator >> select: aBlock [
	^ self select: aBlock as: self defaultItemsClass
]

{ #category : #'mimic various enumerating' }
GTSpotterIterator >> select: aBlock as: aCollectionClass [
	| collection |
	collection := self newAs: aCollectionClass.
	self do: [ :each | 
		(aBlock value: each) 
			ifTrue: [ collection add: each ] ].
	^ collection
]

{ #category : #'mimic various enumerating' }
GTSpotterIterator >> select: aBlock thenCollect: aBlock2 [
	^ self select: aBlock thenCollect: aBlock2 as: self defaultItemsClass
]

{ #category : #'mimic various enumerating' }
GTSpotterIterator >> select: aBlock thenCollect: aBlock2 as: aCollectionClass [
	| collection |
	collection := self newAs: aCollectionClass.
	self select: aBlock thenDo: [ :each | 
		collection add: (aBlock2 value: each) ].
	^ collection
]

{ #category : #'mimic various enumerating' }
GTSpotterIterator >> select: aBlock thenDo: aBlock2 [
	self do: [ :each | 
		(aBlock value: each) 
			ifTrue: [ aBlock2 value: each ] ]
]

{ #category : #'mimic collection' }
GTSpotterIterator >> size [
	^ self items size
]

{ #category : #public }
GTSpotterIterator >> value [
	^ self items
]

{ #category : #public }
GTSpotterIterator >> value: aContext [
	self context: aContext
]

{ #category : #'mimic enumerating' }
GTSpotterIterator >> withIndexDo: aBlock [
	| index |
	index := 0.
	self do: [ :each | aBlock cull: each cull: (index := index + 1) ]
]
