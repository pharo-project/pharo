"
Debugger command tree builder mock to test general behavior outside of a debugger
"
Class {
	#name : #StDebuggerMockCommandTreeBuilder,
	#superclass : #StDebuggerCommandTreeBuilder,
	#category : #'NewTools-Debugger-Tests-Utils'
}

{ #category : #accessing }
StDebuggerMockCommandTreeBuilder class >> displayStrategy [
	^CmUIDisplayAsSubMenu
]

{ #category : #accessing }
StDebuggerMockCommandTreeBuilder class >> groupDescription [
	^'mock group'
]

{ #category : #accessing }
StDebuggerMockCommandTreeBuilder class >> groupName [
	^'mock'
]

{ #category : #building }
StDebuggerMockCommandTreeBuilder >> buildDebuggerCommandGroup [
	
]
