"
I'm an object answering to part of the API of `DebugSession`, but doing nothing.
You can use me in your tests where messages are sent to a debug session but you do not actually want to act on a real one. 
"
Class {
	#name : #StMockSession,
	#superclass : #Object,
	#category : #'NewTools-Debugger-Tests-Utils'
}

{ #category : #'debug - session' }
StMockSession >> implement: aMessage classified: messageCategory inClass: aClass forContext: aContext [

	
]

{ #category : #'debugging actions' }
StMockSession >> restart: aContext [
]

{ #category : #'debugging actions' }
StMockSession >> returnValue: value from: aContext [
]

{ #category : #'debugging actions' }
StMockSession >> runToSelection: aSelectionInterval inContext: aContext [
]

{ #category : #'debugging actions' }
StMockSession >> stepInto: aContext [ 
]

{ #category : #'debugging actions' }
StMockSession >> stepOver: aContext [
]

{ #category : #'debugging actions' }
StMockSession >> stepThrough: aContext [
]
