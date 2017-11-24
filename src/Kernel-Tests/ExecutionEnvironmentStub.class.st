"
I am stub to test ExecutionEnvironment's
"
Class {
	#name : #ExecutionEnvironmentStub,
	#superclass : #ExecutionEnvironment,
	#instVars : [
		'forkedProcesses'
	],
	#category : #'Kernel-Tests-Processes'
}

{ #category : #accessing }
ExecutionEnvironmentStub >> forkedProcesses [
	^ forkedProcesses
]

{ #category : #accessing }
ExecutionEnvironmentStub >> forkedProcesses: anObject [
	forkedProcesses := anObject
]

{ #category : #initialization }
ExecutionEnvironmentStub >> initialize [
	super initialize.
	
	forkedProcesses := OrderedCollection new
]

{ #category : #controlling }
ExecutionEnvironmentStub >> prepareForNewProcess: aProcess [ 
	forkedProcesses add: aProcess
]
