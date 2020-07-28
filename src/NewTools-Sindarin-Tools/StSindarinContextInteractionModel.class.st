Class {
	#name : #StSindarinContextInteractionModel,
	#superclass : #StDebuggerContextInteractionModel,
	#category : #'NewTools-Sindarin-Tools'
}

{ #category : #binding }
StSindarinContextInteractionModel >> bindingOf: aString [
	^ self bindings at: aString ifAbsent: [ super bindingOf: aString ]
]
