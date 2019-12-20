"
SUnit tests for weak value dictionaries
"
Class {
	#name : #WeakValueDictionaryTest,
	#superclass : #DictionaryTest,
	#category : #'Collections-Tests-Weak'
}

{ #category : #'building suites' }
WeakValueDictionaryTest class >> shouldInheritSelectors [

^true
]

{ #category : #coverage }
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
	self assertEmpty: self dictionaryWithGarbageCollectedValue associations
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
WeakValueDictionaryTest >> testAtIfPresent [
	"Non regression test. Before we were sending #value on the value while giving it to the block to evaluate."

	| d block |
	d := self classToBeTested new.
	d at: 1 put: (block := [ 2 ]).

	d at: 1 ifPresent: [ :v | self assert: v identicalTo: block ]
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
WeakValueDictionaryTest >> testIncludesKeyAnswerFalseForGarbageCollectedValues [
	"This test should not be called #testIncludesKey to not override that test of the same name in the superclass."

	self deny: (self dictionaryWithGarbageCollectedValue includesKey: self keyWithGarbageCollectedValue)
]

{ #category : #tests }
WeakValueDictionaryTest >> testKeysAndValuesDoWithGarbageCollectedValue [

	self dictionaryWithGarbageCollectedValue keysAndValuesDo: [:key :value | self fail ]

]

{ #category : #tests }
WeakValueDictionaryTest >> testRehashDoesNotTransformAssociations [
	| d |
	d := self classToBeTested new.
	d at: 1 put: 2.
	self assert: (d privateAssociations anyOne isKindOf: WeakValueAssociation).
	d rehash.
	self assert: (d privateAssociations anyOne isKindOf: WeakValueAssociation)
]

{ #category : #tests }
WeakValueDictionaryTest >> testReturnedAssociationsAreRight [
	| dictionary |
	dictionary := self classToBeTested new.
	dictionary at: 'test' put: nil.
	dictionary at: 'test2' put: 1.
	self assert: (dictionary associationAt: 'test') value isNil.
	self assert: (dictionary associationAt: 'test2') value equals: 1
]

{ #category : #'tests - size capacity' }
WeakValueDictionaryTest >> testSize [
	| d |
	d := self classToBeTested new.
	self assert: d size equals: 0.
	d at: 1 put: 2.
	d at: 2 put: nil.
	self assert: d size equals: 2
]

{ #category : #tests }
WeakValueDictionaryTest >> testSizeWithGarbageCollectedValue [

	self
		assert: self dictionaryWithGarbageCollectedValue size
		equals: 0.

]
