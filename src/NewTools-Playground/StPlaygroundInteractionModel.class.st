Class {
	#name : #StPlaygroundInteractionModel,
	#superclass : #SpCodeScriptingInteractionModel,
	#category : #'NewTools-Playground-View'
}

{ #category : #binding }
StPlaygroundInteractionModel >> addBinding: aBinding [

	(self isGlobalVariable: aBinding key) ifTrue: [ ^ self ].
	bindings add: aBinding
]

{ #category : #accessing }
StPlaygroundInteractionModel >> application [

	^ self owner application
]

{ #category : #binding }
StPlaygroundInteractionModel >> bindingOf: aString [

	"prevent the case when a tool (like completion) asks for a global binding"
	(self class bindingOf: aString) ifNotNil: [ :aBinding | ^ aBinding ].
	"if there is no binding, I will include a key"
	(self bindings includesKey: aString)
		ifFalse: [ self addBinding: (WorkspaceVariable key: aString asSymbol) ].
	^ self bindings associationAt: aString
]

{ #category : #binding }
StPlaygroundInteractionModel >> bindings [
	"Combining shared bindings and local bindings needs to be done like this because the 
	 #add: method of Dictionary stores the whole association (which in this case is a 
	 WorkspaceVariable and we need that. #at:put: cannot be used because it will create 
	 an Association and want to keep the WorkspaceVariable."
	| joinedBindings |
	
	"just a small optimisation"
	self application sharedBindings ifEmpty: [ ^ bindings ].
	
	joinedBindings := self application sharedBindings copy.
	bindings associations 
		do: [ :each | joinedBindings add: each ].
	^ joinedBindings
]

{ #category : #private }
StPlaygroundInteractionModel >> isGlobalVariable: aSymbol [

	^ (self class environment classNamed: aSymbol) notNil
]
