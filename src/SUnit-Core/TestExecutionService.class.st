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
TestExecutionService >> cleanUpAfterTest [

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
TestExecutionService >> handleCompletedTest [
]

{ #category : #controlling }
TestExecutionService >> handleNewProcess: aProcess [
]

{ #category : #controlling }
TestExecutionService >> handleUnhandledException: anUnhandledException [
]
