Class {
	#name : #ExecutionCounterTest,
	#superclass : #TestCase,
	#instVars : [
		'counter',
		'node'
	],
	#category : #'Reflectivity-Tools-Tests'
}

{ #category : #running }
ExecutionCounterTest >> setUp [
	"Hooks that subclasses may override to define the fixture of test."
	super setUp.
	node := (ReflectivityExamples >> #exampleAssignment) ast body children second.
]

{ #category : #running }
ExecutionCounterTest >> tearDown [ 	
	counter ifNotNil:[counter uninstall].
	super tearDown
]

{ #category : #counting }
ExecutionCounterTest >> testCounting [
	counter := ExecutionCounter installOn: node.
	10 timesRepeat:[ReflectivityExamples new exampleAssignment].
	self assert: counter count equals: 10
]

{ #category : #counting }
ExecutionCounterTest >> testCountingAfterReset [
	counter := ExecutionCounter installOn: node.
	10 timesRepeat:[ReflectivityExamples new exampleAssignment].
	counter reset.
	ReflectivityExamples new exampleAssignment.
	self assert: counter count equals: 1
]

{ #category : #adding }
ExecutionCounterTest >> testInstallCounter [
	counter := ExecutionCounter installOn: node.
	self assert: node hasExecutionCounter
]

{ #category : #removing }
ExecutionCounterTest >> testRemoveCounterFromMethod [
	counter := ExecutionCounter installOn: node.
	ExecutionCounter removeFromMethod: node methodNode compiledMethod.
	self deny: node hasExecutionCounter
]

{ #category : #counting }
ExecutionCounterTest >> testReseting [
	counter := ExecutionCounter installOn: node.
	10 timesRepeat:[ReflectivityExamples new exampleAssignment].
	counter reset.
	self assert: counter count equals: 0
]

{ #category : #removing }
ExecutionCounterTest >> testUninstallCounter [
	counter := ExecutionCounter installOn: node.
	counter uninstall.
	self deny: node hasExecutionCounter 
]
