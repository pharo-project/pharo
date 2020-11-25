"
Test to check if there are methods who have unused temporary variables in the image
"
Class {
	#name : #NoUnusedVariablesLeftTest,
	#superclass : #TestCase,
	#category : #'ReleaseTests-CleanCode'
}

{ #category : #accessing }
NoUnusedVariablesLeftTest class >> defaultTimeLimit [

	^ 2 minute
]

{ #category : #testing }
NoUnusedVariablesLeftTest >> testNoUnusedClassVariablesLeft [
	| variables classes validExceptions remaining |
	
	"turn this on when all class variables are cleaned"
	self skip.
	
	variables := Smalltalk globals allBehaviors flatCollect: [ :each | each classVariables ].
	variables := variables reject: [ :each | each isReferenced ].
	
	classes := variables collect: [ :each | each definingClass ] as: Set.
	
	validExceptions := { }.	
	
	remaining := classes asOrderedCollection removeAll: validExceptions; yourself.
	
	self assert: classes isEmpty
]

{ #category : #testing }
NoUnusedVariablesLeftTest >> testNoUnusedInstanceVariablesLeft [
	| variables classes validExceptions remaining |
	
	"turn this on when all instance variables are cleaned"
	self skip.
	
	variables := Smalltalk globals allBehaviors flatCollect: [ :each | each instanceVariables ].
	variables := variables reject: [ :each | each isReferenced ].
	
	classes := variables collect: [ :each | each definingClass ] as: Set.
	
	validExceptions := { }.	
	
	remaining := classes asOrderedCollection removeAll: validExceptions; yourself.
	
	self assert: classes isEmpty
]

{ #category : #testing }
NoUnusedVariablesLeftTest >> testNoUnusedTemporaryVariablesLeft [
	"Fail if there are methods who have unused temporary variables"
	| found validExceptions remaining |
	found := SystemNavigation default allMethodsSelect: [ :m | 
		((m numTemps - m numArgs) > 0) and: [  
		m ast temporaries anySatisfy: [ :x | x binding isUnused ] ] ].
	
	"No other exceptions beside the ones mentioned here should be allowed"	
	validExceptions := { MFClassA>>#method. MFClassB>>#method3. MFClassB>>#method2 . TemporaryVariableTest>>#testIsReferenced }.	
	
	remaining := found asOrderedCollection 
								removeAll: validExceptions;
								yourself.
								
	self assert: remaining isEmpty description: ('the following methods have unused temporary variables and should be cleaned: ', remaining asString)
]
