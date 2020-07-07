Class {
	#name : #SpCodeScriptingInteractionModel,
	#superclass : #SpCodeInteractionModel,
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

{ #category : #initialization }
SpCodeScriptingInteractionModel >> initialize [

	self setBindings: Dictionary new.
	super initialize
]

{ #category : #testing }
SpCodeScriptingInteractionModel >> isForScripting [

	^ true
]

{ #category : #accessing }
SpCodeScriptingInteractionModel >> removeBinding: aBinding [
 
	bindings removeKey: aBinding key
]
