Class {
	#name : #StSindarinContextInteractionModel,
	#superclass : #SpCodeScriptingInteractionModel,
	#instVars : [
		'context'
	],
	#category : #'NewTools-Sindarin-Tools'
}

{ #category : #binding }
StSindarinContextInteractionModel >> bindingOf: aString [
	^ self bindings at: aString ifAbsent: [ 
		  (context receiver class hasSlotNamed: aString)
			  ifTrue: [ context receiver class slotNamed: aString ]
			  ifFalse: [ context temporaryVariableNamed: aString ] ]
]

{ #category : #accessing }
StSindarinContextInteractionModel >> context [
	^ context
]

{ #category : #accessing }
StSindarinContextInteractionModel >> context: anObject [
	context := anObject
]

{ #category : #binding }
StSindarinContextInteractionModel >> hasBindingOf: aString [
	self bindings at: aString ifAbsent: [ 
		^ (context receiver class hasSlotNamed: aString) or: [ 
			  (context temporaryVariableNamed: aString) notNil ] ].
	^ true
]
