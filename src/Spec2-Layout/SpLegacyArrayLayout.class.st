Class {
	#name : #SpLegacyArrayLayout,
	#superclass : #SpLegacyLayout,
	#instVars : [
		'specArray'
	],
	#category : #'Spec2-Layout-Deprecated'
}

{ #category : #accessing }
SpLegacyArrayLayout >> array: aCollection [ 
	
	specArray := aCollection
]

{ #category : #converting }
SpLegacyArrayLayout >> asArray [

	^ specArray
]

{ #category : #converting }
SpLegacyArrayLayout >> selector [

	^ nil
]
