Class {
	#name : #StDebuggerContextInteractionModel,
	#superclass : #SpCodeObjectInteractionModel,
	#instVars : [
		'context'
	],
	#category : #'NewTools-Debugger-Model'
}

{ #category : #'instance creation' }
StDebuggerContextInteractionModel class >> on: aContext [

	^ self new context: aContext
]

{ #category : #accessing }
StDebuggerContextInteractionModel >> behavior [

	^ self context receiver class
]

{ #category : #accessing }
StDebuggerContextInteractionModel >> context [

	^context
]

{ #category : #accessing }
StDebuggerContextInteractionModel >> context: aContext [
	context := aContext
]

{ #category : #accessing }
StDebuggerContextInteractionModel >> doItContext [

	^ self context
]

{ #category : #accessing }
StDebuggerContextInteractionModel >> doItReceiver [

	^ self context receiver
]

{ #category : #accessing }
StDebuggerContextInteractionModel >> object [

	^ self doItReceiver
]
