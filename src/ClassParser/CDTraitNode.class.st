"
I represent a simple Trait. (no composition, alias, exclusion)
"
Class {
	#name : #CDTraitNode,
	#superclass : #CDAbstractTraitCompositionNode,
	#instVars : [
		'name'
	],
	#category : #'ClassParser-Model'
}

{ #category : #testing }
CDTraitNode >> isTraitNode [

	^ true
]

{ #category : #accessing }
CDTraitNode >> name [
	^name 
]

{ #category : #accessing }
CDTraitNode >> name: aString [ 
	name := aString
]

{ #category : #printing }
CDTraitNode >> toString [ 
	^ self name
]
