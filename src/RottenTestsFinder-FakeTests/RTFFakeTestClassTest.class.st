Class {
	#name : 'RTFFakeTestClassTest',
	#superclass : 'RTFFakeTestSuperClassTest',
	#category : 'RottenTestsFinder-FakeTests',
	#package : 'RottenTestsFinder-FakeTests'
}

{ #category : 'wrapper' }
RTFFakeTestClassTest >> assertHelper [
	self assert: true
]

{ #category : 'wrapper' }
RTFFakeTestClassTest >> helperHelper [
	self assertHelper
]

{ #category : 'wrapper' }
RTFFakeTestClassTest >> rottenHelper [
	false ifTrue: [ self assert: true ]
]

{ #category : 'tests' }
RTFFakeTestClassTest >> testCallingSuperHelper [
	self superAssertHelper
]

{ #category : 'tests' }
RTFFakeTestClassTest >> testFailingButNotConsideredAsRotten [

	self assert: false.

	self assert: true
]

{ #category : 'tests' }
RTFFakeTestClassTest >> testHelperHelperAssertHelper [
	
	self helperHelper; assertHelper 
]

{ #category : 'tests' }
RTFFakeTestClassTest >> testWithAssertionInHelperRun [
	self assertHelper
]

{ #category : 'tests' }
RTFFakeTestClassTest >> testWithAssertionInSuperHelperRun [
	self superAssertHelper
]

{ #category : 'tests' }
RTFFakeTestClassTest >> testWithAssertionRun [
	self assert: true
]

{ #category : 'tests' }
RTFFakeTestClassTest >> testWithHelperHelper [
	self helperHelper
]

{ #category : 'tests' }
RTFFakeTestClassTest >> testWithNoAssertionInHelperRun [
	self rottenHelper
]

{ #category : 'tests' }
RTFFakeTestClassTest >> testWithNoAssertionInSuperHelperRun [
	self superNotAssertHelper
]

{ #category : 'tests' }
RTFFakeTestClassTest >> testWithoutAssertionRun [
	false
		ifTrue: [ self assert: true ]
]
