"
I represent a result of the computation of the SpaceTally 

I store SpaceTallyItems for each class. I interact with SpaceTally.

Public API and Key Messages

- item   
- total* methods 

 
Internal Representation and Key Implementation Points.

    Instance Variables
	items:		<aCollectionOfSpaceTallyItem>

Implementation Points

My total* methods could be cached for perf
"
Class {
	#name : #SpaceTallyResult,
	#superclass : #Object,
	#instVars : [
		'items'
	],
	#category : #'Tool-Profilers-Space'
}

{ #category : #'instance creation' }
SpaceTallyResult class >> new: aSize [
	^ self new
		itemSize: aSize;
		yourself
]

{ #category : #adding }
SpaceTallyResult >> addItem: aSpaceTallyItem [
	items add: aSpaceTallyItem
]

{ #category : #initialization }
SpaceTallyResult >> initialize [
	super initialize.
	items := OrderedCollection new
]

{ #category : #initialization }
SpaceTallyResult >> itemSize: aSize [
	items := OrderedCollection new: aSize
]

{ #category : #accessing }
SpaceTallyResult >> items [
	^ items
]

{ #category : #accessing }
SpaceTallyResult >> itemsForThreshold: threshold [
	| totalInstSpace |
	threshold <= 0
		ifTrue: [ ^ self items ].
	totalInstSpace := self totalInstSpace.
	^(self items
		select: [ :s | 
			s instanceCount >= threshold
				or: [ s spaceForInstances > (totalInstSpace // 500) ] ])
		asSortedCollection: [ :s :s2 | s spaceForInstances > s2 spaceForInstances ]
]

{ #category : #accessing }
SpaceTallyResult >> totalCodeSpace [
	^ self items sum: #codeSize
]

{ #category : #accessing }
SpaceTallyResult >> totalInstCount [
	^ self items sum: #instanceCount
]

{ #category : #accessing }
SpaceTallyResult >> totalInstSpace [
	^ self items sum: #spaceForInstances
]
