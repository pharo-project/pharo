"
I execute a test case with a given block
"
Class {
	#name : 'ExecutionBlock',
	#superclass : 'ExecutionMode',
	#instVars : [
		'executionBlock'
	],
	#category : 'TestRunner-Core-ExecutionModes',
	#package : 'TestRunner-Core',
	#tag : 'ExecutionModes'
}

{ #category : 'instance creation' }
ExecutionBlock class >> executionBlock: aBlock [

	^ self new
		executionBlock: aBlock;
		yourself
]

{ #category : 'execution' }
ExecutionBlock >> executeCase: aTestCase [

	^ executionBlock value: aTestCase 
]

{ #category : 'accessing' }
ExecutionBlock >> executionBlock: anObject [

	executionBlock := anObject
]
