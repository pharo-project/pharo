Class {
	#name : 'RTFFakeTestClass',
	#superclass : 'RTFFakeTestSuperClass',
	#category : 'RottenTestsFinder-FakeTests',
	#package : 'RottenTestsFinder-FakeTests'
}

{ #category : 'wrapper' }
RTFFakeTestClass >> assertHelper [
	self assert: true
]

{ #category : 'wrapper' }
RTFFakeTestClass >> helperHelper [
	self assertHelper
]

{ #category : 'wrapper' }
RTFFakeTestClass >> rottenHelper [
	false ifTrue: [ self assert: true ]
]

{ #category : 'tests' }
RTFFakeTestClass >> testCallingSuperHelper [
	self superAssertHelper
]

{ #category : 'tests' }
RTFFakeTestClass >> testFailingButNotConsideredAsRotten [

	self assert: false.

	self assert: true
]

{ #category : 'tests' }
RTFFakeTestClass >> testWithAssertionInHelperRun [
	self assertHelper
]

{ #category : 'tests' }
RTFFakeTestClass >> testWithAssertionInSuperHelperRun [
	self superAssertHelper
]

{ #category : 'tests' }
RTFFakeTestClass >> testWithAssertionRun [
	self assert: true
]

{ #category : 'tests' }
RTFFakeTestClass >> testWithHelperHelper [
	self helperHelper
]

{ #category : 'tests' }
RTFFakeTestClass >> testWithNoAssertionInHelperRun [
	self rottenHelper
]

{ #category : 'tests' }
RTFFakeTestClass >> testWithNoAssertionInSuperHelperRun [
	self superNotAssertHelper
]

{ #category : 'tests' }
RTFFakeTestClass >> testWithoutAssertionRun [
	false
		ifTrue: [ self assert: true ]
]
