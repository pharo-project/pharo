Class {
	#name : 'ReBlockFormattingRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Formatting',
	#package : 'General-Rules-Tests',
	#tag : 'Formatting'
}

{ #category : 'tests' }
ReBlockFormattingRuleTest >> testRuleForBlocksWithCorrectSpaces [

	| critiques |
	self class
		compile: 'blocksWithCorrectSpaces

	#() do: [ :n | n ].
	#() do: [ :n | #() do: [ :n2 | n2 ] ].
	#() do: [ :n |
		n foo ]'
		classified: 'test-helper'.
	[
	critiques := self myCritiquesOnMethod: self class >> #blocksWithCorrectSpaces.
	self assertEmpty: critiques  ] ensure: [
		(self class >> #blocksWithCorrectSpaces) removeFromSystem ]
]

{ #category : 'tests' }
ReBlockFormattingRuleTest >> testRuleForBlocksWithNoSpaces [

	| critiques |
	self class
		compile: 'blocksWithNoSpaces

	#() do: [:n|n].
	#() do: [ :n | #() do: [:n2|] ].
	#() do: [:n|
		n foo]'
		classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #blocksWithNoSpaces.
	self assert: critiques size equals: 3 ] ensure: [ (self class >> #blocksWithNoSpaces)  removeFromSystem ]
]

{ #category : 'tests' }
ReBlockFormattingRuleTest >> testRuleForblocksWithInconsistentSpaces [

	| critiques |
	self class
		compile: 'blocksWithInconsistentSpaces

	#() do: [ :n|n].
	#() do: [:n|n ].
	#() do: [:n| n].
	#() do: [ :n |
		n foo]'
		classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #blocksWithInconsistentSpaces.
	self assert: critiques size equals: 4 ] ensure: [ (self class >> #blocksWithInconsistentSpaces) removeFromSystem ]
]
