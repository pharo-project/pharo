"
A ValueLinkTest is a test class for testing the behavior of ValueLink
"
Class {
	#name : 'ValueLinkTest',
	#superclass : 'TestCase',
	#category : 'Collections-Support-Tests-Links',
	#package : 'Collections-Support-Tests',
	#tag : 'Links'
}

{ #category : 'tests' }
ValueLinkTest >> testNilAsLink [

	self assert: nil asLink equals: (ValueLink value: nil)
]

{ #category : 'tests' }
ValueLinkTest >> testPushingOnObjects [

	| link o |
	o := Object new.
	link := 3 ~~> (#aSymbol ~~> o).

	self
		assert: link value equals: 3;
		assert: link nextLink value equals: #aSymbol;
		assert: link nextLink nextLink value equals: o;
		assert: link nextLink nextLink nextLink equals: nil
]

{ #category : 'tests' }
ValueLinkTest >> testPushingOnValueLinks [

	| link link1 link2 link3 |
	link := 3 ~~> nil.
	link1 := 5 ~~> link.
	link2 := ValueLinkForTesting new
		         value: 8;
		         nextLink: link1;
		         yourself.
	link3 := link1 value + link2 value ~~> link2.

	self
		assert: link3 value equals: 13;
		assert: link3 class equals: ValueLinkForTesting;
		assert: link3 nextLink value equals: 8;
		assert: link3 nextLink class equals: ValueLinkForTesting;
		assert: link3 nextLink nextLink value equals: 5;
		assert: link3 nextLink nextLink class equals: ValueLink;
		assert: link3 nextLink nextLink nextLink value equals: 3;
		assert: link3 nextLink nextLink nextLink class equals: ValueLink;
		assert: link3 nextLink nextLink nextLink nextLink equals: nil
]
