"
SUnit tests for weak value dictionaries
"
Class {
	#name : #WeakValueDictionaryTest,
	#superclass : #DictionaryTest,
	#category : #Collections-Tests-Weak
}

{ #category : #'building suites' }
WeakValueDictionaryTest class >> shouldInheritSelectors [

^true
]

{ #category : #requirements }
WeakValueDictionaryTest >> classToBeTested [

^ WeakValueDictionary
]

{ #category : #accessing }
WeakValueDictionaryTest >> dictionaryWithGarbageCollectedValue [

	| dictionary |
	dictionary := self classToBeTested new.
	dictionary at: self keyWithGarbageCollectedValue put: Object new.
	Smalltalk garbageCollect.
	^ dictionary

]

{ #category : #accessing }
WeakValueDictionaryTest >> keyWithGarbageCollectedValue [

	^ #a
]

{ #category : #tests }
WeakValueDictionaryTest >> testAssociationsWithGarbageCollectedValue [

	self assert: self dictionaryWithGarbageCollectedValue associations isEmpty

]

{ #category : #tests }
WeakValueDictionaryTest >> testAtGarbageCollectedValueRaisesNotFound [

	self
		should: [ self dictionaryWithGarbageCollectedValue at: self keyWithGarbageCollectedValue ]
		raise: NotFound

]

{ #category : #tests }
WeakValueDictionaryTest >> testAtIfAbsentPutWithGarbageCollectedValue [

	| dictionary |
	dictionary := self dictionaryWithGarbageCollectedValue.

	self
		assert:
			(dictionary
				at: self keyWithGarbageCollectedValue
				ifAbsentPut: [ 42 ])
		equals: 42.
		
	self assert: (dictionary includes: 42).

]

{ #category : #tests }
WeakValueDictionaryTest >> testAtIfAbsentWithGarbageCollectedValue [

	self
		assert:
			(self dictionaryWithGarbageCollectedValue
				at: self keyWithGarbageCollectedValue
				ifAbsent: [ 42 ])
		equals: 42

]

{ #category : #tests }
WeakValueDictionaryTest >> testAtIfPresentIfAbsentWithGarbageCollectedValue [

	self
		assert:
			(self dictionaryWithGarbageCollectedValue
				at: self keyWithGarbageCollectedValue
				ifPresent: [ 41 ]
				ifAbsent: [ 42 ])
		equals: 42

]

{ #category : #tests }
WeakValueDictionaryTest >> testAtIfPresentWithGarbageCollectedValue [

	| dictionary |
	dictionary := self dictionaryWithGarbageCollectedValue.

	self
		assert:
			(dictionary
				at: self keyWithGarbageCollectedValue
				ifPresent: [ 42 ])
		equals: nil.

]

{ #category : #tests }
WeakValueDictionaryTest >> testDoesNotIncludeNilAfterGarbageCollection [

	self deny: (self dictionaryWithGarbageCollectedValue includes: nil)
]

{ #category : #tests }
WeakValueDictionaryTest >> testKeysAndValuesDoWithGarbageCollectedValue [

	self dictionaryWithGarbageCollectedValue keysAndValuesDo: [:key :value | self fail ]

]

{ #category : #tests }
WeakValueDictionaryTest >> testSizeWithGarbageCollectedValue [

	self
		assert: self dictionaryWithGarbageCollectedValue size
		equals: 0.

]
