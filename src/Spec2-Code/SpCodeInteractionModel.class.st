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
		'owner'
	],
	#category : #'Spec2-Code'
}

{ #category : #accessing }
SpCodeInteractionModel >> behavior [

	^ nil
]

{ #category : #binding }
SpCodeInteractionModel >> bindingOf: aString [

	^ self subclassResponsibility
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

	^ self subclassResponsibility
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

{ #category : #accessing }
SpCodeInteractionModel >> variableBindingNames [
	
	^ self variableBindings keys
]

{ #category : #accessing }
SpCodeInteractionModel >> variableBindings [

	^ Dictionary new
]
