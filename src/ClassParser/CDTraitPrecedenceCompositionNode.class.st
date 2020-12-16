"
I represent the trait composition node used when an sequence of traits has to define precedence for one of the traits.
I am a syntax sugar to declare traits, as my behavior is exactly the same than removing all the messages of the prefered trait from the rest of the composition.
"
Class {
	#name : #CDTraitPrecedenceCompositionNode,
	#superclass : #CDTraitCompositionSequenceNode,
	#instVars : [
		'preferedTrait'
	],
	#category : #'ClassParser-Model'
}

{ #category : #accessing }
CDTraitPrecedenceCompositionNode >> preferedTrait [
	^ preferedTrait
]

{ #category : #accessing }
CDTraitPrecedenceCompositionNode >> preferedTrait: anObject [
	preferedTrait := anObject
]
