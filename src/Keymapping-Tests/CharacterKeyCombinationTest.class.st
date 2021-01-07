Class {
	#name : #CharacterKeyCombinationTest,
	#superclass : #TestCase,
	#category : #'Keymapping-Tests'
}

{ #category : #tests }
CharacterKeyCombinationTest >> testLowercaseKeyCombination [
	| combination |
	combination := $a asKeyCombination.
	self assert: combination key equals: KeyboardKey A.
	self assert: combination modifier equals: KMNoModifier new.
]

{ #category : #tests }
CharacterKeyCombinationTest >> testUppercaseKeyCombination [
	| combination |
	combination := $A asKeyCombination.
	self assert: combination key equals: KeyboardKey A.
	self assert: combination modifier equals: KMModifier shift.
]
