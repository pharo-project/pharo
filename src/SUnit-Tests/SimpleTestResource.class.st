"
I'm a simple test ressource for test purposes
"
Class {
	#name : #SimpleTestResource,
	#superclass : #TestResource,
	#instVars : [
		'runningState',
		'hasRun',
		'hasSetup'
	],
	#classInstVars : [
		'preventAvailability'
	],
	#category : #'SUnit-Tests-Resources'
}

{ #category : #accessing }
SimpleTestResource class >> allowAvailability [
	^preventAvailability isNil
]

{ #category : #utilties }
SimpleTestResource class >> preventAvailabilityDuring: aBlock [
	"Only setter of preventAvailability."
	
	preventAvailability := false.
	^aBlock ensure: [preventAvailability := nil]
]

{ #category : #accessing }
SimpleTestResource class >> rawCurrentForTest [
	^current
]

{ #category : #testing }
SimpleTestResource >> hasRun [
	^hasRun
]

{ #category : #testing }
SimpleTestResource >> hasSetup [
	^hasSetup
]

{ #category : #testing }
SimpleTestResource >> isAvailable [
	^self class allowAvailability
		and: [self runningState == self startedStateSymbol]
]

{ #category : #accessing }
SimpleTestResource >> runningState [

	^runningState
]

{ #category : #accessing }
SimpleTestResource >> runningState: aSymbol [

	runningState := aSymbol
]

{ #category : #running }
SimpleTestResource >> setRun [
	hasRun := true
]

{ #category : #running }
SimpleTestResource >> setUp [
	super setUp.
	self runningState: self startedStateSymbol.
	hasSetup := true
]

{ #category : #running }
SimpleTestResource >> startedStateSymbol [

	^#started
]

{ #category : #running }
SimpleTestResource >> stoppedStateSymbol [

	^#stopped
]

{ #category : #running }
SimpleTestResource >> tearDown [

	self runningState: self stoppedStateSymbol
]
