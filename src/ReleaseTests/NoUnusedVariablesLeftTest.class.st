"
Test to check if there are methods who have unused temporary variables in the image
"
Class {
	#name : 'NoUnusedVariablesLeftTest',
	#superclass : 'TestCase',
	#category : 'ReleaseTests-CleanCode',
	#package : 'ReleaseTests',
	#tag : 'CleanCode'
}

{ #category : 'accessing' }
NoUnusedVariablesLeftTest class >> defaultTimeLimit [

	^ 2 minute
]

{ #category : 'testing' }
NoUnusedVariablesLeftTest >> testNoUnusedClassVariablesLeft [

	| variables classes |
	variables := Smalltalk globals allClassesAndTraits flatCollect: [ :class | class classVariables ].

	variables := variables reject: [ :variable |
		             variable isReferenced or: [
			             variable definingClass pragmas anySatisfy: [ :pragma |
				             pragma selector = #ignoreUnusedClassVariables: and: [ (pragma argumentAt: 1) includes: variable name ] ] ] ].

	classes := variables collect: [ :each | each definingClass ] as: Set.

	self assert: classes isEmpty description: 'the following classes have unused class variables and should be cleaned: ' , classes asOrderedCollection asString
]

{ #category : 'testing' }
NoUnusedVariablesLeftTest >> testNoUnusedInstanceVariablesLeft [

	| variables classes |
	variables := Smalltalk globals allBehaviors flatCollect: [ :class | class instanceVariables ].

	variables := variables reject: [ :variable |
		             variable isReferenced or: [
			             variable definingClass pragmas anySatisfy: [ :pragma |
				             pragma selector = #ignoreUnusedVariables: and: [ (pragma argumentAt: 1) includes: variable name ] ] ] ].

	classes := variables collect: [ :each | each definingClass ] as: Set.

	self assert: classes isEmpty description: 'the following classes have unused instance variables and should be cleaned: ' , classes asOrderedCollection asString
]

{ #category : 'testing' }
NoUnusedVariablesLeftTest >> testNoUnusedTemporaryVariablesLeft [
	"Fail if there are methods who have unused temporary variables"
	| found validExceptions |
	found := SystemNavigation default allMethodsSelect: [ :m |
		m hasTemporaries and: [m ast temporaries anySatisfy: [ :x | x binding isUsed not] ] ].

	"No other exceptions beside the ones mentioned here should be allowed"
	validExceptions := { MFClassA>>#method. MFClassB>>#method3. MFClassB>>#method2 . TemporaryVariableTest>>#testIsReferenced }.
	found removeAll: validExceptions.

	self assert: found isEmpty description: ('the following methods have unused temporary variables and should be cleaned: ', found asString)
]
