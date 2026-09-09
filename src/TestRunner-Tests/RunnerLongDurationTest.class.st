Class {
	#name : 'RunnerLongDurationTest',
	#superclass : 'TestCase',
	#instVars : [
		'runner'
	],
	#category : 'TestRunner-Tests-Core',
	#package : 'TestRunner-Tests',
	#tag : 'Core'
}

{ #category : 'accessing' }
RunnerLongDurationTest >> longTestClass [

	^ LongTest
]

{ #category : 'running' }
RunnerLongDurationTest >> setUp [

	super setUp.
	self longTestClass doNotIgnore.
	runner := NewTestRunner new suitePreparator:
		          ShuffleSuitePreparator new 
]

{ #category : 'running' }
RunnerLongDurationTest >> tearDown [

	self longTestClass ignore.
	super tearDown 
]

{ #category : 'tests' }
RunnerLongDurationTest >> testRunningFromAClass [

	self skip.
	runner runClass: self longTestClass.
	self assert: runner runCount equals: 1.
	self assert: runner passedCount equals: 1
]

{ #category : 'tests' }
RunnerLongDurationTest >> testRunningFromACollectionOfClasses [

	self skip.
	runner runClasses: { LongTest . LongTest . LongTest }.
	self assert: runner runCount equals: 3.
	self assert: runner passedCount equals: 3
]
