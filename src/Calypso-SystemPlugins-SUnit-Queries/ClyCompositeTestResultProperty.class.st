"
I compose test result information from all classes which inherit tests from decorated method or class.
My counts variables provide summary on all of them. And the map testCaseResults include information for each of them.

I am used to correctly run inherited tests when they are visible in the browser from subclasses.
And I allow to run all tests of abstract class including all subclasses.
   
Internal Representation and Key Implementation Points.

    Instance Variables
	testCaseResults:		<Dictionary of<TestCase class, ClyTestResultProperty>>
"
Class {
	#name : #ClyCompositeTestResultProperty,
	#superclass : #ClyTestResultProperty,
	#instVars : [
		'testCaseResults'
	],
	#category : #'Calypso-SystemPlugins-SUnit-Queries'
}

{ #category : #accessing }
ClyCompositeTestResultProperty >> accumulateTestMethod: aMethod [
	| testSelector result inheritingScope |	
	testSelector := aMethod selector.
	inheritingScope := (ClyClassScope of: aMethod methodClass) withInheritingScope.
	inheritingScope classesDo: [ :concreteTestCase | 
		(concreteTestCase isTestCase and: [concreteTestCase isAbstract not]) ifTrue: [
			self accumulateTest: testSelector of: concreteTestCase.
			
			result := testCaseResults 
				at: concreteTestCase ifAbsentPut: [ ClyTestResultProperty new ].
			result accumulateTest: testSelector of: concreteTestCase ]]
]

{ #category : #accessing }
ClyCompositeTestResultProperty >> concreteResultOf: aTestClass ifPresent: presentBlock [

	^testCaseResults at: aTestClass ifPresent: presentBlock
]

{ #category : #initialization }
ClyCompositeTestResultProperty >> initialize [
	super initialize.
	
	testCaseResults := IdentityDictionary new
]

{ #category : #accessing }
ClyCompositeTestResultProperty >> testCaseResults [
	^ testCaseResults
]

{ #category : #accessing }
ClyCompositeTestResultProperty >> testCaseResults: anObject [
	testCaseResults := anObject
]

{ #category : #accessing }
ClyCompositeTestResultProperty >> testCases [
	^ testCaseResults keys
]
