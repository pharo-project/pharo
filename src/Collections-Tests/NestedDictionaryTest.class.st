"
SUnit test class for nested dictionaries
"
Class {
	#name : #NestedDictionaryTest,
	#superclass : #TestCase,
	#category : #'Collections-Tests-Unordered'
}

{ #category : #tests }
NestedDictionaryTest >> testAtAtIfAbsent [
	| d |
	d := Dictionary new.
	d at: #top at: #below1 put: 1.
	d at: #top at: #below1 put: 2.
	d at: #fop at: #below1 put: 1.
	self assert: (d at: #top at: #below1) equals: 2.
	self assert: (d at: #fop at: #below666 ifAbsent: [ 666 ]) equals: 666.
	self assert: (d at: #fop666 at: #below1 ifAbsent: [ 666 ]) equals: 666.
	"when the first key is not found do not execute the second one."
	self assert: (d at: #fop666 at: [ 1 / 0 ] ifAbsent: [ 666 ]) equals: 666
]

{ #category : #tests }
NestedDictionaryTest >> testAtAtIfAbsentPut [
	| d |
	d := Dictionary new.
	d at: #first at: #second put: [ 2 ].
	d at: #first at: #second42 ifAbsentPut: [ 142 ].
	d at: #first42 at: #second ifAbsentPut: [ 42 ].
	d at: #first42 at: #second42 ifAbsentPut: [ 0 ].
	self assert: (d at: #first at: #second42) equals: 142.
	self assert: (d at: #first42 at: #second) equals: 42.
	self assert: (d at: #first42 at: #second42) equals: 0.
	
]

{ #category : #tests }
NestedDictionaryTest >> testAtAtOnIdentityDictinary [

	| dct key1 key2 subDct |

	dct := IdentityDictionary new.
	key1 := 'a'.
	key2 := key1 copy.
	dct
		at: #a at: key1 put: 1;
		at: #a at: key2 put: 2.
	subDct := dct at: #a.
	TestCase new
		assert: subDct size equals: 2;
		assert: (subDct at: key1) equals: 1;
		assert: (subDct at: key2) equals: 2
]

{ #category : #tests }
NestedDictionaryTest >> testAtAtPut [
	| d |
	d := Dictionary new.
	d at: #top at: #below1 put: 1.
	d at: #top at: #below1 put: 2.
	d at: #fop at: #below1 put: 1.
	self assert: (d at: #top at: #below1) equals: 2.
	self assert: (d at: #fop at: #below1) equals: 1
]

{ #category : #tests }
NestedDictionaryTest >> testAtAtputEffectivelyPutAValue [
	| d |
	d := Dictionary new.
	d at: #top at: #below1 put: 1.
	self assert: (d at: #top at: #below1) equals: 1
]

{ #category : #tests }
NestedDictionaryTest >> testAtAtputHasLastPutLastStayBehavior [
	| d |
	d := Dictionary new.
	d at: #top at: #below1 put: 1.
	d at: #top at: #below1 put: 2.
	self assert: (d at: #top at: #below1) equals: 2
]
