Class {
	#name : 'CDTraitPrecedenceCompositionNode',
	#superclass : 'CDTraitCompositionSequenceNode',
	#instVars : [
		'preferedTrait'
	],
	#category : 'ClassParser-Model',
	#package : 'ClassParser',
	#tag : 'Model'
}

{ #category : 'accessing' }
CDTraitPrecedenceCompositionNode >> preferedTrait [
	^ preferedTrait
]

{ #category : 'accessing' }
CDTraitPrecedenceCompositionNode >> preferedTrait: anObject [
	preferedTrait := anObject
]
