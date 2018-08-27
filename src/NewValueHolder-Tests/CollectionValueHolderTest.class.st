"
Unit tests for collection value holders
"
Class {
	#name : #CollectionValueHolderTest,
	#superclass : #TestCase,
	#instVars : [
		'empty',
		'withItems',
		'fired'
	],
	#category : #'NewValueHolder-Tests'
}

{ #category : #running }
CollectionValueHolderTest >> setUp [
	super setUp.
	empty := OrderedCollection new asValueHolder.
	withItems := #(1 2 3) asOrderedCollection asValueHolder.
	fired := false
]

{ #category : #tests }
CollectionValueHolderTest >> testAddMany [
	empty
		whenAddedDo:
			[ :addedValue :ann | 
			fired := true.
			self assert: addedValue equals: #(1 2).
			self assert: (ann isMemberOf: ValueAdded) ].
	empty addAll: #(1 2).
	self assert: fired
]

{ #category : #tests }
CollectionValueHolderTest >> testAddOne [
	empty
		whenAddedDo:
			[ :addedValue :ann | 
			fired := true.
			self assert: addedValue equals: 1.
			self assert: (ann isMemberOf: ValueAdded) ].
	empty add: 1.
	self assert: fired
]

{ #category : #tests }
CollectionValueHolderTest >> testChangeAddMany [
	empty
		whenChangedDo:
			[ :col :changed | 
			fired := true.
			self assert: col asArray equals: #(1 2 3).
			self assert: changed equals: #(1 2 3) ].
	empty addAll: #(1 2 3).
	self assert: fired
]

{ #category : #tests }
CollectionValueHolderTest >> testChangeAddOne [
	empty
		whenChangedDo:
			[ :col :changed | 
			fired := true.
			self assert: col asArray equals: #(1).
			self assert: changed equals: 1 ].
	empty add: 1.
	self assert: fired
]

{ #category : #tests }
CollectionValueHolderTest >> testChangeRemoveMany [
	withItems
		whenChangedDo:
			[ :col :changed | 
			fired := true.
			self assert: col asArray equals: #(2).
			self assert: changed equals: #(1 3) ].
	withItems removeAll: #(1 3).
	self assert: fired
]

{ #category : #tests }
CollectionValueHolderTest >> testChangeRemoveOne [
	withItems
		whenChangedDo:
			[ :col :changed | 
			fired := true.
			self assert: col asArray equals: #(2 3).
			self assert: changed equals: 1 ].
	withItems remove: 1.
	self assert: fired
]

{ #category : #tests }
CollectionValueHolderTest >> testRemoveMany [
	withItems
		whenRemovedDo:
			[ :removedValue :ann | 
			fired := true.
			self assert: withItems asArray equals: #(2).
			self assert: removedValue equals: #(1 3).
			self assert: (ann isMemberOf: ValueRemoved) ].
	withItems removeAll: #(1 3).
	self assert: fired
]

{ #category : #tests }
CollectionValueHolderTest >> testRemoveOne [
	withItems
		whenRemovedDo:
			[ :removedValue :ann | 
			fired := true.
			self assert: withItems asArray equals: #(2 3).
			self assert: removedValue equals: 1.
			self assert: (ann isMemberOf: ValueRemoved) ].
	withItems remove: 1.
	self assert: fired
]
