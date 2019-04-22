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

{ #category : #accessing }
CDTraitCompositionNode >> subject [ 
	^subject
]

{ #category : #accessing }
CDTraitCompositionNode >> subject: aNode [

	subject := aNode
]
