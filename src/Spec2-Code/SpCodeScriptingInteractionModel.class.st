Class {
	#name : #SpCodeScriptingInteractionModel,
	#superclass : #SpCodeInteractionModel,
	#instVars : [
		'bindings'
	],
	#category : #'Spec2-Code'
}

{ #category : #accessing }
SpCodeScriptingInteractionModel >> addBinding: aBinding [

	bindings add: aBinding
]

{ #category : #binding }
SpCodeScriptingInteractionModel >> bindingOf: aString [

	"if there is no binding, I will include a key"
	(self bindings includesKey: aString)
		ifFalse: [ self addBinding: (WorkspaceVariable key: aString asSymbol) ].
	^ self bindings associationAt: aString
]

{ #category : #accessing }
SpCodeScriptingInteractionModel >> bindings [

	^ bindings ifNil: [ bindings := Dictionary new ]
]

{ #category : #testing }
SpCodeScriptingInteractionModel >> hasBindingOf: aString [

	^ self bindings includesKey: aString asSymbol
]

{ #category : #testing }
SpCodeScriptingInteractionModel >> isForScripting [

	^ true
]

{ #category : #accessing }
SpCodeScriptingInteractionModel >> removeBinding: aBinding [
 
	bindings removeKey: aBinding key
]

{ #category : #accessing }
SpCodeScriptingInteractionModel >> variableBindings [

	^ self bindings
]
