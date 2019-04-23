"
Testing class
"
Class {
	#name : #PaAbstractExampleTest,
	#superclass : #ParametrizedTestCase,
	#instVars : [
		'option2',
		'option1'
	],
	#category : #'ParametrizedTests-Tests'
}

{ #category : #private }
PaAbstractExampleTest class >> isAbstract [
	^ self name = #PaAbstractExampleTest
]

{ #category : #accessing }
PaAbstractExampleTest >> option1 [
	^ option1
]

{ #category : #accessing }
PaAbstractExampleTest >> option1: aString [ 
	option1 := aString
]

{ #category : #accessing }
PaAbstractExampleTest >> option2 [
	^ option2
]

{ #category : #accessing }
PaAbstractExampleTest >> option2: anInteger [ 
	option2 := anInteger
]

{ #category : #accessing }
PaAbstractExampleTest >> testParametersHaveValue [
	self assert: option1 isNotNil.
	self assert: option2 isNotNil.	
]
