"
I am a dummy counterpart to the DebuggerSystem class, used to test it.
"
Class {
	#name : #OupsDummyDebuggerSystem,
	#superclass : #OupsDebuggerSystem,
	#instVars : [
		'newUIProcessSpawned',
		'customUIManager'
	],
	#category : #'Debugger-Oups-Tests-utils'
}

{ #category : #'default values' }
OupsDummyDebuggerSystem >> customUIManager: aUIManager [
	customUIManager := aUIManager
]

{ #category : #'default values' }
OupsDummyDebuggerSystem >> debuggerSelectionStrategy [
	^ OupsDummySelectionStrategy new
]

{ #category : #'default values' }
OupsDummyDebuggerSystem >> defaultUIManager [
	customUIManager ifNil: [ ^ super defaultUIManager ].
	^ customUIManager
]

{ #category : #accessing }
OupsDummyDebuggerSystem >> newUIProcessSpawned [
	newUIProcessSpawned ifNil: [ newUIProcessSpawned := false ].
	^ newUIProcessSpawned
]

{ #category : #helpers }
OupsDummyDebuggerSystem >> spawnNewUIProcess [ 
	"For testing purposes. Does not actually spawn a new UI process. Just record that this method was called"
	newUIProcessSpawned := true
]

{ #category : #helpers }
OupsDummyDebuggerSystem >> suspendDebuggedProcess: aDebugRequest [
	"Do not actually suspend the debugged process, in order to prevent the UI process from being suspended (image freeze) if a test goes wrong"
	^ 1
]
