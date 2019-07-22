"
A MorphWrapper is used to wrap a morph with a layout in the goal to be easily added to another morph
"
Class {
	#name : #SpMorphWrapper,
	#superclass : #Object,
	#instVars : [
		'layout',
		'morph'
	],
	#category : #'Spec2-Adapters-Morphic-Support'
}

{ #category : #'instance creation' }
SpMorphWrapper class >> morph: morph layout: aLayout [ 

	^ self new
		layout: aLayout;
		morph: morph;
		yourself
]

{ #category : #adding }
SpMorphWrapper >> addIn: aContainer [
		
	aContainer
		addMorph: morph
		fullFrame: layout
]

{ #category : #accessing }
SpMorphWrapper >> frame: rectangle [
	
	layout := rectangle asSpLayoutFrame
]

{ #category : #accessing }
SpMorphWrapper >> fullFrame: anObject [
	
	layout := anObject
]

{ #category : #accessing }
SpMorphWrapper >> morph: anObject [
	
	morph := anObject
]
