Class {
	#name : #SpecOverlayConstraints,
	#superclass : #SpecLayoutConstraints,
	#instVars : [
		'overlay'
	],
	#category : #'Spec-Layout-Overlay'
}

{ #category : #accessing }
SpecOverlayConstraints >> beOverlay [
	
	^ overlay := true
]

{ #category : #testing }
SpecOverlayConstraints >> isOverlay [
	
	^ overlay = true
]
