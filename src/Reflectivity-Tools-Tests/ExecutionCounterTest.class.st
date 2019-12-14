Class {
	#name : #ExecutionCounterTest,
	#superclass : #TestCase,
	#instVars : [
		'counter',
		'counter2',
		'node',
		'node2'
	],
	#category : #'Reflectivity-Tools-Tests'
}

{ #category : #running }
ExecutionCounterTest >> setUp [
	"Hooks that subclasses may override to define the fixture of test."
	super setUp.
	node := (ReflectivityExamples >> #exampleAssignment) ast body children second.
	node2 := (ReflectivityExamples >> #exampleAssignment) ast body children last
]

{ #category : #running }
ExecutionCounterTest >> tearDown [ 	
	counter ifNotNil:[counter uninstall].
	counter2 ifNotNil:[counter2 uninstall].
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
ExecutionCounterTest >> testGlobalCounterReferenceAfterInstallation [
	counter := ExecutionCounter installOn: node.
	self assert: (ExecutionCounter allCounters includesKey: node).
	self assert: (ExecutionCounter allCounters at: node) identicalTo: counter.
	self assert: (counter link methods includes: node methodNode compiledMethod)
]

{ #category : #removing }
ExecutionCounterTest >> testGlobalCounterReferenceAfterRemovingFromMethod [
	counter := ExecutionCounter installOn: node.
	counter2 := ExecutionCounter installOn: node2.
	ExecutionCounter removeFromMethod: node methodNode compiledMethod.
	self deny: (ExecutionCounter allCounters includesKey: node).
	self deny: (ExecutionCounter allCounters includesKey: node2).
	self assertEmpty: counter link methods.
	self assertEmpty: counter2 link methods
]

{ #category : #removing }
ExecutionCounterTest >> testGlobalCounterReferenceAfterUninstallInstallation [
	counter := ExecutionCounter installOn: node.
	counter uninstall.
	self deny: (ExecutionCounter allCounters includesKey: node).
	self assertEmpty: counter link methods
]

{ #category : #adding }
ExecutionCounterTest >> testInstallCounter [
	counter := ExecutionCounter installOn: node.
	self assert: node hasExecutionCounter
]

{ #category : #counting }
ExecutionCounterTest >> testMultipleCounters [
	counter := ExecutionCounter installOn: node.
	counter2 := ExecutionCounter installOn: node2.
	10 timesRepeat:[ReflectivityExamples new exampleAssignment].
	self assert: counter count equals: 10.
	self assert: counter2 count equals: 10
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
