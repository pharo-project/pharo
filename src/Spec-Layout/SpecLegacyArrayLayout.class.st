Class {
	#name : #SpecLegacyArrayLayout,
	#superclass : #SpecLegacyLayout,
	#instVars : [
		'specArray'
	],
	#category : #'Spec-Layout-Layouts'
}

{ #category : #accessing }
SpecLegacyArrayLayout >> array: aCollection [ 
	
	specArray := aCollection
]

{ #category : #converting }
SpecLegacyArrayLayout >> asArray [

	^ specArray
]

{ #category : #converting }
SpecLegacyArrayLayout >> selector [

	^ nil
]
