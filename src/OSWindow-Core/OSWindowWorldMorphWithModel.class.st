"
I am an OSWindow world morph that includes a model, to be used with Spec and Glamour.
"
Class {
	#name : #OSWindowWorldMorphWithModel,
	#superclass : #OSWindowWorldMorph,
	#instVars : [
		'model'
	],
	#category : #'OSWindow-Core-Morphic'
}

{ #category : #accessing }
OSWindowWorldMorphWithModel >> model [ 
    ^ model
]

{ #category : #accessing }
OSWindowWorldMorphWithModel >> model: anObject [
    "Set my model and make me me a dependent of the given object."

    model ifNotNil: [model removeDependent: self].
    anObject ifNotNil: [anObject addDependent: self].
    model := anObject.
]
