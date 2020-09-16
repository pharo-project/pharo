"
I am a dummy object to test StDebuggerContext inspector nodes building.
"
Class {
	#name : #StDebuggerObjectForTests,
	#superclass : #Object,
	#instVars : [
		'instVar'
	],
	#category : #'NewTools-Debugger-Tests-Utils'
}

{ #category : #initialization }
StDebuggerObjectForTests >> initialize [ 
	instVar := 0
]

{ #category : #accessing }
StDebuggerObjectForTests >> instVar [
	^ instVar
]

{ #category : #util }
StDebuggerObjectForTests >> methodWithArg1: i arg2: j [
	|a b|
	a := i.
	b := j.	
	^a + b
]

{ #category : #util }
StDebuggerObjectForTests >> methodWithArgsInBlock [
	^[ :a :b| a + b ] value: 4 value: 2
]

{ #category : #util }
StDebuggerObjectForTests >> methodWithTemps [
	|a b|
	a := 0.
	b := 0.
	^a + b
]
