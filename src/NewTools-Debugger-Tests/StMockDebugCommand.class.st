"
A mock command for dezbugger command tree builder tests
"
Class {
	#name : #StMockDebugCommand,
	#superclass : #StDebuggerCommand,
	#category : #'NewTools-Debugger-Tests-Utils'
}

{ #category : #initialization }
StMockDebugCommand class >> defaultIconName [
	^ #defaultIconName
]

{ #category : #initialization }
StMockDebugCommand class >> defaultName [

	^ UUID new printString
]

{ #category : #initialization }
StMockDebugCommand class >> defaultShortcut [
	^ #defaultShortcut
]

{ #category : #executing }
StMockDebugCommand >> execute [
	^self context
]
