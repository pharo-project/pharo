"
SUnit test class for nested dictionaries
"
Class {
	#name : #NestedDictionaryTest,
	#superclass : #TestCase,
	#category : #Collections-Tests-Unordered
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
