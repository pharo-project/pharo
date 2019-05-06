"
MyTrait - {#aliasName}
"
Class {
	#name : #CDTraitExclusionNode,
	#superclass : #CDTraitCompositionNode,
	#instVars : [
		'exclusions'
	],
	#category : #'ClassParser-Model'
}

{ #category : #accessing }
CDTraitExclusionNode >> exclusions: aCollection [ 
	exclusions := aCollection
]

{ #category : #testing }
CDTraitExclusionNode >> isTraitExclusionNode [
	
	^ true
]

{ #category : #printing }
CDTraitExclusionNode >> toString [
	"should replace Exclusions"
	^ subject name ,' - ', exclusions asString.
]
