"
I'm the model for a typed completion, that means when a receiver class is known. I only have selectors. I have an expand toggle: When false I filter out Object and ProtoObject selectors. The default value is true.
"
Class {
	#name : #NECTypedModel,
	#superclass : #NECModel,
	#instVars : [
		'expanded'
	],
	#category : #'NECompletion-Model'
}

{ #category : #initialization }
NECTypedModel >> initialize [
	super initialize.
	expanded := true
]

{ #category : #private }
NECTypedModel >> initializeSelectors [
	self initializeSelectorsFor: clazz
]

{ #category : #private }
NECTypedModel >> initializeSelectorsFor: aClass [ 
	|excludedClasses|
	selectors reset.
	excludedClasses := (expanded ifTrue: [#()] ifFalse: [Object withAllSuperclasses]).
	selectors addAll: ((aClass realClass allSelectorsWithout: excludedClasses)
						collect: [:each | NECSelectorEntry contents: each type: #selector])
]

{ #category : #action }
NECTypedModel >> narrowWith: aString [ 
	self
		narrowString: aString ;
		initializeSelectors.
	entries reset.
	self addToEntries: selectors
]

{ #category : #action }
NECTypedModel >> theClass [
	^clazz
]

{ #category : #action }
NECTypedModel >> title [
	^clazz name
]

{ #category : #action }
NECTypedModel >> toggleExpand [
	expanded := expanded not.
	self initializeSelectors.
	self narrowWith: narrowString
]
