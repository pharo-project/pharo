Class {
	#name : #StDebuggerContextInteractionModel,
	#superclass : #SpCodeInteractionModel,
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

{ #category : #binding }
StDebuggerContextInteractionModel >> bindingOf: aString [

	^ self doItReceiver bindingOf: aString
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

{ #category : #testing }
StDebuggerContextInteractionModel >> hasBindingOf: aString [

	^ self doItReceiver hasBindingOf: aString
]

{ #category : #accessing }
StDebuggerContextInteractionModel >> object [

	^ self doItReceiver
]
