Class {
	#name : #TestExecutionService,
	#superclass : #Object,
	#instVars : [
		'executionEnvironment'
	],
	#category : #'SUnit-Core-Kernel'
}

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
