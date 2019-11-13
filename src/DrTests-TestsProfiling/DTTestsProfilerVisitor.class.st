"
I collect the time for every executed test.
I create a DTTestCaseProfilingData for each test and store all of them in a collection.
"
Class {
	#name : #DTTestsProfilerVisitor,
	#superclass : #SUnitVisitor,
	#instVars : [
		'profilingData'
	],
	#category : #'DrTests-TestsProfiling'
}

{ #category : #initialization }
DTTestsProfilerVisitor >> initialize [
	super initialize.
	self profilingData: OrderedCollection new
]

{ #category : #accessing }
DTTestsProfilerVisitor >> profilingData [
	^ profilingData
]

{ #category : #accessing }
DTTestsProfilerVisitor >> profilingData: anObject [
	profilingData := anObject
]

{ #category : #visiting }
DTTestsProfilerVisitor >> visitTestCase: aTestCase [
	"Defines the behaviour while visiting a TestCase.
	 This method must be overriden by concrete subclasses.
	"

	| aTestresult |
	self profilingData
		add:
			(DTTestCaseProfilingData
				testCase: aTestCase
				duration: [ aTestresult := aTestCase run ] timeToRun
				testResult: aTestresult) 
]
