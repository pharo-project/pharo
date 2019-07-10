"
I mark methods which has covering test. 
I include information about result of such test.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	coveringTest:		<TestCase>
	testResult:		<ClyTestResultProperty>
"
Class {
	#name : #ClyTestedMethodProperty,
	#superclass : #ClyProperty,
	#instVars : [
		'coveringTest',
		'testResult'
	],
	#category : #'Calypso-SystemPlugins-SUnit-Queries'
}

{ #category : #'instance creation' }
ClyTestedMethodProperty class >> coveringTest: aMethodTest result: aTestResultProperty [

	^self new 
		coveringTest: aMethodTest;
		testResult: aTestResultProperty 
]

{ #category : #accessing }
ClyTestedMethodProperty >> coveringTest [
	^ coveringTest
]

{ #category : #accessing }
ClyTestedMethodProperty >> coveringTest: anObject [
	coveringTest := anObject
]

{ #category : #accessing }
ClyTestedMethodProperty >> testResult [
	^ testResult
]

{ #category : #accessing }
ClyTestedMethodProperty >> testResult: anObject [
	testResult := anObject
]
