"
A ValueLinkTest is a test class for testing the behavior of ValueLink
"
Class {
	#name : #ValueLinkTest,
	#superclass : #TestCase,
	#category : #'Collections-Support-Tests-Links'
}

{ #category : #tests }
ValueLinkTest >> testNilAsLink [

	self assert: nil asLink equals: (ValueLink value: nil)
]

{ #category : #tests }
ValueLinkTest >> testPushingOnValueLink [

	| link link1 link2 link3 |
	link := 3 ~~> nil.
	link1 := 5 ~~> link.
	link2 := ValueLinkForTesting value: 8.
	link3 := link1 value + link2 value ~~> link2.

	self
		assert: link1 class equals: ValueLink;
		assert: link3 class equals: ValueLinkForTesting;
		assert: link3 value equals: 13
]
