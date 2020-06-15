"
SUnit tests for SpaceTally
"
Class {
	#name : #SpaceTallyTest,
	#superclass : #TestCase,
	#category : #'Tool-Profilers-Tests-Space'
}

{ #category : #tests }
SpaceTallyTest >> test2Classes [
	| result |
	result := (SpaceTally new
		spaceTally: (Array with: TextMorph with: Point)) items
		asSortedCollection: [ :a :b | a spaceForInstances > b spaceForInstances ].
	self assert: result size equals: 2.
	self
		assertCollection: (result collect: [:each | each analyzedClassName])
		hasSameElements: #(#Point #TextMorph).
	self
		assert:
			result anyOne codeSize + result anyOne instanceCount
				+ result anyOne spaceForInstances > 0
]

{ #category : #tests }
SpaceTallyTest >> testPrintOnStream [
	| contents |
	contents := String
		streamContents: [ :s | SpaceTally printSpaceAnalysis: 1 on: s ].
	self assert: contents isString
]

{ #category : #tests }
SpaceTallyTest >> testSystemWide [
	self
		assert:
			(SpaceTally new systemWideSpaceTally
				asSortedCollection: [ :a :b | a spaceForInstances > b spaceForInstances ])
				asArray isNotEmpty
]
