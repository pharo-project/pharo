"
A NautilusUITest is a test class for testing the behavior of NautilusUI
"
Class {
	#name : #NautilusUITest,
	#superclass : #TestCase,
	#instVars : [
		'nautilusUI',
		'nautilusUI2',
		'nautilusUI3'
	],
	#category : #'Nautilus-Tests'
}

{ #category : #running }
NautilusUITest >> setUp [
	"Setting up code for NautilusUITest"

	super setUp.
	nautilusUI := NautilusUI on: Nautilus new.
]

{ #category : #running }
NautilusUITest >> tearDown [
	"Tearing down code for NautilusUITest"

	nautilusUI := nil.
	super tearDown 
]

{ #category : #tests }
NautilusUITest >> testBuildTestSelectorFor [

	| method selector |
	method := MorphicEventHandler >> #keyStroke:fromMorph:.
	selector := nautilusUI buildTestSelectorFor: method.
	self assert: selector equals: 'testKeyStrokeFromMorph'
]

{ #category : #tests }
NautilusUITest >> testMethodSortBlock [
	"Test that the default sort block sorts binary methods ahead of
	non-binary methods and sorts using Character values otherwise."

	| sortBlock |
	sortBlock := nautilusUI methodSortBlock.

	"These classes define at leat one binary selector, #~= or #|, which
	comes after any non-binary selector when sorted by Character value."
	{Object.
	Collection}
		do: [ :class | 
			| methods binaryMethods nonBinaryMethods sortedMethods |
			methods := class methods.
			binaryMethods := methods select: [ :each | each selector isBinary ].
			nonBinaryMethods := methods reject: [ :each | each selector isBinary ].
			sortedMethods :=
				(binaryMethods sorted: [ :a :b | a selector <= b selector ]),
					(nonBinaryMethods sorted: [ :a :b | a selector <= b selector ]).
			self
				assert: (methods sorted: sortBlock) = sortedMethods;
				assert: (methods reversed sorted: sortBlock) = sortedMethods;
				assert: (sortedMethods sorted: sortBlock) = sortedMethods;
				assert: (sortedMethods reversed sorted: sortBlock) = sortedMethods ]
]
