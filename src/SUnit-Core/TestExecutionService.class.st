Class {
	#name : #TestExecutionService,
	#superclass : #Object,
	#instVars : [
		'executionEnvironment'
	],
	#category : #'SUnit-Core-Kernel'
}

{ #category : #'instance creation' }
TestExecutionService class >> for: aTestExecutionEnvironment [
	^self new 
		executionEnvironment: aTestExecutionEnvironment 
]

{ #category : #controlling }
TestExecutionService >> cleanUpAfterTest: aTestCase [

]

{ #category : #accessing }
TestExecutionService >> executionEnvironment [
	^ executionEnvironment
]

{ #category : #accessing }
TestExecutionService >> executionEnvironment: anObject [
	executionEnvironment := anObject
]

{ #category : #controlling }
TestExecutionService >> handleCompletedTest: aTestCase [
]

{ #category : #controlling }
TestExecutionService >> handleError: anError duringTest: aTestCase [
]

{ #category : #controlling }
TestExecutionService >> handleHalt: aHalt duringTest: aTestCase [

]

{ #category : #controlling }
TestExecutionService >> handleNewProcess: aProcess [
]
