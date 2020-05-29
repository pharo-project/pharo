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
