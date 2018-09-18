"
I'm the model for a typed completion, that means when a receiver class is known. I only have selectors.
"
Class {
	#name : #NECTypedModel,
	#superclass : #NECModel,
	#category : #'NECompletion-Model'
}

{ #category : #private }
NECTypedModel >> initializeSelectors [
	self initializeSelectorsFor: clazz
]

{ #category : #private }
NECTypedModel >> initializeSelectorsFor: aClass [
	selectors reset.
	selectors addAll:
			(aClass allSelectors collect: [ :each | NECSelectorEntry contents: each type: #selector ]).
	self sortSelectors.
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
