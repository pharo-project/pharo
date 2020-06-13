"
Test to check if there are methods who have unused temporary variables in the image
"
Class {
	#name : #NoUnusedTemporaryVariablesLeftTest,
	#superclass : #TestCase,
	#category : #'ReleaseTests-CleanCode'
}

{ #category : #accessing }
NoUnusedTemporaryVariablesLeftTest class >> defaultTimeLimit [

	^ 2 minute
]

{ #category : #running }
NoUnusedTemporaryVariablesLeftTest >> tearDown [
	ASTCache reset.
	super tearDown
]

{ #category : #testing }
NoUnusedTemporaryVariablesLeftTest >> testNoUnusedTemporaryVariablesLeft [
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
