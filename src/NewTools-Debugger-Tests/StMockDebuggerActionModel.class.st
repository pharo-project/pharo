"
I'm used to mock certain aspects of the `StDebuggerActionModel` class for testing.
"
Class {
	#name : #StMockDebuggerActionModel,
	#superclass : #StDebuggerActionModel,
	#instVars : [
		'tag'
	],
	#category : #'NewTools-Debugger-Tests-Utils'
}

{ #category : #'debug - session' }
StMockDebuggerActionModel >> autoClassifyMessage: aMessage inClass: aClass [
]

{ #category : #context }
StMockDebuggerActionModel >> computeInitialTopContext [
]

{ #category : #accessing }
StMockDebuggerActionModel >> session [
	^StMockSession new
]

{ #category : #accessing }
StMockDebuggerActionModel >> tag [
	^tag
	
]

{ #category : #'debug - stepping' }
StMockDebuggerActionModel >> updateTopContext [
	tag :=  #updateTopContext
	
]
