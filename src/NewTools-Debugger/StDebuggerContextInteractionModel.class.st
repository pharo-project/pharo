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

	^ (context lookupVar: aString) ifNotNil: [ :var | 
		  var asDoItVariableFrom: context ]
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

	^ (context lookupVar: aString) notNil
]

{ #category : #testing }
StDebuggerContextInteractionModel >> hasUnsavedCodeChanges [

	^ context notNil and: [ context sourceCode ~= owner text asString ]
]

{ #category : #accessing }
StDebuggerContextInteractionModel >> object [

	^ self doItReceiver
]
