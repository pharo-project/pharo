Class {
	#name : 'CoMiniTestCase',
	#superclass : 'TestCase',
	#instVars : [
		'package',
		'completionClass',
		'systemNavigation',
		'fetcher',
		'expectedElements'
	],
	#category : 'HeuristicCompletion-Tests-Core',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Core'
}

{ #category : 'assert' }
CoMiniTestCase >> assertEntry: anEntry equals: expectedEntry [

	"Entries do not define equality :("
	self assert: anEntry class equals: expectedEntry class.
	self assert: anEntry contents equals: expectedEntry contents
]

{ #category : 'accessing' }
CoMiniTestCase >> fetcher [ 

	^ fetcher
]

{ #category : 'accessing' }
CoMiniTestCase >> firstExpectedElement [
	^ expectedElements first
]

{ #category : 'accessing' }
CoMiniTestCase >> secondExpectedElement [
	^ expectedElements second
]
