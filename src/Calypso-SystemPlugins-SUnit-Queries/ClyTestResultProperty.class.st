"
I provide information about result of running tests which are related to my environment item.

My instances are created from method ot test case: 

	ClyTestResultProperty ofMethod: aCompiledMethod.
	ClyTestResultProperty ofMethod: aTestCaseClass.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	allCount:		<Integer>
	errorsCount:		<Integer>
	failuresCount:		<Integer>
	successesCount:		<Integer>
"
Class {
	#name : #ClyTestResultProperty,
	#superclass : #ClyProperty,
	#instVars : [
		'allCount',
		'errorCount',
		'failureCount',
		'successCount'
	],
	#category : #'Calypso-SystemPlugins-SUnit-Queries'
}

{ #category : #'instance creation' }
ClyTestResultProperty class >> ofMethod: aMethod [
	^ClyCompositeTestResultProperty new 
		accumulateTestMethod: aMethod
]

{ #category : #'instance creation' }
ClyTestResultProperty class >> ofTestCase: aTestCaseClass [
	^self new 
		accumulateTestCase: aTestCaseClass
]

{ #category : #accessing }
ClyTestResultProperty >> accumulateFailures: failures andErrors: errors [

	allCount := allCount + failures + errors.
	failureCount := failureCount + failures.
	errorCount := errorCount + errors
]

{ #category : #accessing }
ClyTestResultProperty >> accumulateTest: testSelector of: aTestClass [

	allCount := allCount + 1.
	successCount := successCount + (aTestClass methodPassed: testSelector) asBit.
	failureCount := failureCount + (aTestClass methodFailed: testSelector) asBit.
	errorCount := errorCount + (aTestClass methodRaisedError: testSelector) asBit
]

{ #category : #accessing }
ClyTestResultProperty >> accumulateTestCase: aClass [

	allCount := allCount + aClass allTestSelectors size.
	successCount := successCount + (aClass history at: #passed) size.
	failureCount := failureCount + (aClass history at: #failures) size.
	errorCount := errorCount + (aClass history at: #errors) size
]

{ #category : #accessing }
ClyTestResultProperty >> accumulateTestMethod: testMethod [

	self accumulateTest: testMethod selector of: testMethod methodClass
]

{ #category : #accessing }
ClyTestResultProperty >> allCount [
	^ allCount
]

{ #category : #accessing }
ClyTestResultProperty >> allCount: anObject [
	allCount := anObject
]

{ #category : #accessing }
ClyTestResultProperty >> brokenCount [
	^failureCount + errorCount 
]

{ #category : #presentation }
ClyTestResultProperty >> createIcon [

	allCount = 0 ifTrue: [ ^ self iconNamed: #testNotRunIcon ].
	allCount = successCount
		ifTrue: [ ^ self iconNamed: #testGreenIcon ].
	errorCount = 0 & (failureCount > 0)
		ifTrue: [ ^ self iconNamed: #testYellowIcon ].
	errorCount > 0
		ifTrue: [ ^ self iconNamed: #testRedIcon ].
		
	^ self iconNamed: #testNotRunIcon
]

{ #category : #accessing }
ClyTestResultProperty >> errorCount [
	^ errorCount
]

{ #category : #accessing }
ClyTestResultProperty >> errorCount: anObject [
	errorCount := anObject
]

{ #category : #accessing }
ClyTestResultProperty >> failureCount [
	^ failureCount
]

{ #category : #accessing }
ClyTestResultProperty >> failureCount: anObject [
	failureCount := anObject
]

{ #category : #testing }
ClyTestResultProperty >> hasBrokenTests [
	^failureCount > 0 | (errorCount > 0)
]

{ #category : #initialization }
ClyTestResultProperty >> initialize [
	super initialize.
	allCount := 0.
	successCount := 0.
	failureCount := 0.
	errorCount := 0
]

{ #category : #accessing }
ClyTestResultProperty >> successCount [
	^ successCount
]

{ #category : #accessing }
ClyTestResultProperty >> successCount: anObject [
	successCount := anObject
]
