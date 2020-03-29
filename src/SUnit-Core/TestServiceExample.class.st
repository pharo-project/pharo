Class {
	#name : #TestServiceExample,
	#superclass : #TestExecutionService,
	#instVars : [
		'forkedProcesses',
		'terminatedTests',
		'completedTests',
		'haltedTests',
		'failedTests'
	],
	#category : #'SUnit-Core-Kernel'
}

{ #category : #controlling }
TestServiceExample >> cleanUpAfterTest: aTestCase [

	terminatedTests add: aTestCase
]

{ #category : #accessing }
TestServiceExample >> completedTests [
	^ completedTests
]

{ #category : #accessing }
TestServiceExample >> completedTests: anObject [
	completedTests := anObject
]

{ #category : #accessing }
TestServiceExample >> failedTests [
	^ failedTests
]

{ #category : #accessing }
TestServiceExample >> failedTests: anObject [
	failedTests := anObject
]

{ #category : #accessing }
TestServiceExample >> forkedProcesses [
	^ forkedProcesses
]

{ #category : #accessing }
TestServiceExample >> forkedProcesses: anObject [
	forkedProcesses := anObject
]

{ #category : #accessing }
TestServiceExample >> haltedTests [
	^ haltedTests
]

{ #category : #accessing }
TestServiceExample >> haltedTests: anObject [
	haltedTests := anObject
]

{ #category : #controlling }
TestServiceExample >> handleCompletedTest: aTestCase [

	completedTests add: aTestCase
]

{ #category : #controlling }
TestServiceExample >> handleError: anError duringTest: aTestCase [

	failedTests add: aTestCase
]

{ #category : #controlling }
TestServiceExample >> handleHalt: aHalt duringTest: aTestCase [

	haltedTests add: aTestCase
]

{ #category : #controlling }
TestServiceExample >> handleNewProcess: aProcess [

	forkedProcesses add: aProcess
]

{ #category : #initialization }
TestServiceExample >> initialize [ 
	forkedProcesses := OrderedCollection new.
	completedTests := OrderedCollection new.
	terminatedTests := OrderedCollection new.
	haltedTests := OrderedCollection new.
	failedTests := OrderedCollection new.
]

{ #category : #accessing }
TestServiceExample >> terminatedTests [
	^ terminatedTests
]

{ #category : #accessing }
TestServiceExample >> terminatedTests: anObject [
	terminatedTests := anObject
]
