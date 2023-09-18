"
Operations on traits
	- exclusion
	- +
	- alias
	
"
Class {
	#name : #CDTraitCompositionNode,
	#superclass : #CDAbstractTraitCompositionNode,
	#instVars : [
		'subject'
	],
	#category : #'ClassParser-Model'
}

{ #category : #testing }
CDTraitCompositionNode class >> isAbstract [

	^ self == CDTraitCompositionNode
]

{ #category : #accessing }
CDTraitCompositionNode >> subject [
	^subject
]

{ #category : #accessing }
CDTraitCompositionNode >> subject: aNode [

	subject := aNode
]
