"
@@TODO

A simple playground would be defined as: 

[[[language=Smalltalk
SpCodeInteractionMode newForScripting.
]]]

A code editor on a method: 

[[[language=Smalltalk
SpCodeInteractionMode newForMethod: aMethod.
]]]l 
 
"
Class {
	#name : #SpCodeInteractionModel,
	#superclass : #Object,
	#instVars : [
		'owner',
		'bindings'
	],
	#category : #'Spec2-Code'
}

{ #category : #accessing }
SpCodeInteractionModel >> behavior [

	^ nil
]

{ #category : #binding }
SpCodeInteractionModel >> bindingOf: aString [

	^ self bindings 
		associationAt: aString 
		ifAbsent: [ nil ]
]

{ #category : #accessing }
SpCodeInteractionModel >> bindings [

	^ bindings ifNil: [ bindings := Dictionary new ]
]

{ #category : #accessing }
SpCodeInteractionModel >> doItContext [

	^ nil
]

{ #category : #accessing }
SpCodeInteractionModel >> doItReceiver [

	^ nil
]

{ #category : #testing }
SpCodeInteractionModel >> hasBindingOf: aString [

	^ self bindings includesKey: aString asSymbol
]

{ #category : #testing }
SpCodeInteractionModel >> hasBindingThatBeginsWith: aString [

	^ self variableBindingNames 
		anySatisfy: [ :each | each beginsWith: aString ]
]

{ #category : #testing }
SpCodeInteractionModel >> isForScripting [

	^ false
]

{ #category : #accessing }
SpCodeInteractionModel >> owner [

	^ owner
]

{ #category : #accessing }
SpCodeInteractionModel >> owner: aPresenter [

	owner := aPresenter
]

{ #category : #accessing }
SpCodeInteractionModel >> selectedBehavior [

	^ self behavior
]

{ #category : #accessing }
SpCodeInteractionModel >> selectedClass [

	^ self behavior
]

{ #category : #accessing }
SpCodeInteractionModel >> selectedClassOrMetaClass [

	^ self behavior
]

{ #category : #binding }
SpCodeInteractionModel >> setBindings: aDictionary [
	"Sets the Workspace to use the specified dictionary as its namespace"

	bindings := aDictionary
]

{ #category : #accessing }
SpCodeInteractionModel >> variableBindingNames [
	
	^ self bindings keys
]
