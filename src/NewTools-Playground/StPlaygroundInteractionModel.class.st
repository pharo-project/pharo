Class {
	#name : #StPlaygroundInteractionModel,
	#superclass : #SpCodeScriptingInteractionModel,
	#category : #'NewTools-Playground-Model'
}

{ #category : #binding }
StPlaygroundInteractionModel >> addBinding: aBinding [

	(self isGlobalVariable: aBinding key) ifTrue: [ ^ self ].
	bindings ifNil: [ bindings := Dictionary new ].
	bindings add: aBinding
]

{ #category : #binding }
StPlaygroundInteractionModel >> addBindings: aCollection [

	aCollection do: [ :each | self addBinding: each ]
]

{ #category : #accessing }
StPlaygroundInteractionModel >> application [

	^ self owner application
]

{ #category : #binding }
StPlaygroundInteractionModel >> bindingOf: aString [
	| key |

	key := aString asSymbol.
	"prevent the case when a tool (like completion) asks for a global binding"
	(self class bindingOf: key) ifNotNil: [ :aBinding | ^ aBinding ].
	"Look in shared"
	(self sharedBindings includesKey: key)
		ifTrue: [ ^ self sharedBindings associationAt: key ].
	"if there is no binding, I will include a key"
	(self localBindings includesKey: key)
		ifFalse: [ self addBinding: (WorkspaceVariable key: key) ].
	^ self localBindings associationAt: key
]

{ #category : #binding }
StPlaygroundInteractionModel >> bindings [
	"Combining shared bindings and local bindings needs to be done like this because the 
	 #add: method of Dictionary stores the whole association (which in this case is a 
	 WorkspaceVariable and we need that. #at:put: cannot be used because it will create 
	 an Association and want to keep the WorkspaceVariable."
	| joinedBindings |
	
	"just a small optimisation"
	self application sharedBindings ifEmpty: [ ^ self localBindings ].

	joinedBindings := self application sharedBindings copy.
	self localBindings associations 
		do: [ :each | joinedBindings add: each ].

	^ joinedBindings
]

{ #category : #binding }
StPlaygroundInteractionModel >> hasBindingOf: aString [

	^ (self sharedBindings includesKey: aString asSymbol)
		or: [ (self localBindings includesKey: aString asSymbol) ]
]

{ #category : #private }
StPlaygroundInteractionModel >> isGlobalVariable: aSymbol [

	^ (self class environment classNamed: aSymbol) notNil
]

{ #category : #accessing }
StPlaygroundInteractionModel >> localBindings [
	
	^ super bindings


]

{ #category : #accessing }
StPlaygroundInteractionModel >> sharedBindings [
	
	^ self application sharedBindings 


]
