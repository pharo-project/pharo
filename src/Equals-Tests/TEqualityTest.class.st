Class {
	#name : #TEqualityTest,
	#superclass : #TestCase,
	#instVars : [
		'classFactory'
	],
	#category : #'Equals-Tests'
}

{ #category : #running }
TEqualityTest >> setUp [
	super setUp.
	classFactory := ClassFactoryForTestCase new
]

{ #category : #running }
TEqualityTest >> tearDown [
	classFactory cleanUp.
	super tearDown
]

{ #category : #testing }
TEqualityTest >> testSetContainsOnlyOneInstanceOfAClassWithIVs [
	| set object class |
	class  := classFactory newSubclassOf: ComparableObjectForEqualityTest  instanceVariableNames: 'x y' classVariableNames:  ''.
	class compile: 'x: newX y: newY
		x := newX.
		y := newY'.
	class class compile: 'x: newX y: newY
		^self new
			x: newX y: newY;
			yourself'.
	set := Set new.
	
	10 timesRepeat: [
		object  := class x: 1 y: 2.
		set add: object].
	
	self assert: set size equals: 1.

]

{ #category : #testing }
TEqualityTest >> testSetContainsOnlyOneInstanceOfAClassWithNoIVs [
	| set |
	set := Set new.
	
	10 timesRepeat: [set add: ComparableObjectForEqualityTest new].
	
	self assert: set size equals: 1.
	self assert: (set includes: ComparableObjectForEqualityTest new)

]

{ #category : #testing }
TEqualityTest >> testSetContainsTwoInstancesOfTwoDiffrentClassesWithIVs [
	| set otherClass yetAnotherClass |
	otherClass := classFactory newSubclassOf: ComparableObjectForEqualityTest  instanceVariableNames: 'x y' classVariableNames:  ''.
	yetAnotherClass := classFactory newSubclassOf: ComparableObjectForEqualityTest  instanceVariableNames: 'x y' classVariableNames:  ''.
	{otherClass. yetAnotherClass} do: [:class|
		class compile: 'x: newX y: newY
			x := newX.
			y := newY'.
		class class compile: 'x: newX y: newY
			^self new
				x: newX y: newY;
				yourself'.
	].

	set := Set with: (otherClass x: 'abc' y: 123) with: (yetAnotherClass x: 'abc' y: 123).
	
	self assert: set size equals: 2.
	self assert: (set includes: (otherClass x: 'abc' y: 123)).
	self assert: (set includes: (yetAnotherClass x: 'abc' y: 123)).

]

{ #category : #testing }
TEqualityTest >> testSetContainsTwoInstancesOfTwoDiffrentClassesWithNoIVs [

	| set otherClass |
	otherClass := classFactory
		              newSubclassOf: ComparableObjectForEqualityTest
		              instanceVariableNames: ''
		              classVariableNames: ''.
		
	set := Set
		       with: ComparableObjectForEqualityTest new
		       with: otherClass new.
		
	self assert: set size equals: 2.
	self assert: (set includes: ComparableObjectForEqualityTest new).
	self assert: (set includes: otherClass new)
]

{ #category : #testing }
TEqualityTest >> testTwoInstancesOfTheSameClassWithDifferentIvValuessAreNotEqual [
	| class |
	class  := classFactory newSubclassOf: ComparableObjectForEqualityTest  instanceVariableNames: 'x y' classVariableNames:  ''.
	class compile: 'x: newX y: newY
		x := newX.
		y := newY'.
	class class compile: 'x: newX y: newY
		^self new
			x: newX y: newY;
			yourself'.
			
	self 
		deny: (class x: 1 y: 'abc') 
		equals: (class x: 1 y: 'zyx').
]

{ #category : #testing }
TEqualityTest >> testTwoInstancesOfTheSameClassWithNoIVsAreEqual [

	self
		assert: ComparableObjectForEqualityTest new
		equals: ComparableObjectForEqualityTest new
]

{ #category : #testing }
TEqualityTest >> testTwoInstancesOfTheSameClassWithSameIvValuessAreEqual [
	| class |
	class  := classFactory newSubclassOf: ComparableObjectForEqualityTest  instanceVariableNames: 'x y' classVariableNames:  ''.
	class compile: 'x: newX y: newY
		x := newX.
		y := newY'.
	class class compile: 'x: newX y: newY
		^self new
			x: newX y: newY;
			yourself'.
			
	self 
		assert: (class x: 1 y: 'abc') 
		equals: (class x: 1 y: 'abc').
]

{ #category : #testing }
TEqualityTest >> testTwoInstancesOfTowDifferentClassesWithNoIVsAreNotEqual [
	| otherClass |
	otherClass := classFactory newSubclassOf: ComparableObjectForEqualityTest  instanceVariableNames: '' classVariableNames:  ''.
	
	self 
		deny: otherClass new 
		equals: ComparableObjectForEqualityTest new
]
