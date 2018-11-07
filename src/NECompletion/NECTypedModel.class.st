"
I'm the model for a typed completion, that means when a receiver class is known. I only have selectors.
"
Class {
	#name : #NECTypedModel,
	#superclass : #NECModel,
	#category : #'NECompletion-Model'
}

{ #category : #initialization }
NECTypedModel >> initializeSelectors [
	selectors reset.
	selectors addAll:
			(clazz allSelectors collect: [ :each | NECSelectorEntry contents: each type: #selector ]).
	self sortSelectors.
]

{ #category : #acessing }
NECTypedModel >> theClass [
	^clazz
]

{ #category : #acessing }
NECTypedModel >> title [
	^clazz name
]
