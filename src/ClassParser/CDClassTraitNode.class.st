"
I represent the class side of a Trait. I may dissapear. All trait are instances of MetaclassForTraits.
"
Class {
	#name : #CDClassTraitNode,
	#superclass : #CDAbstractTraitCompositionNode,
	#instVars : [
		'name'
	],
	#category : #'ClassParser-Model'
}

{ #category : #testing }
CDClassTraitNode >> isTraitNode [

	^ true
]

{ #category : #accessing }
CDClassTraitNode >> name: aString [ 
	name := aString
]
