Class {
	#name : #SpOverlayConstraints,
	#superclass : #SpLayoutConstraints,
	#traits : 'SpTAlignable',
	#instVars : [
		'overlay'
	],
	#category : #'Spec2-Layout-Overlay'
}

{ #category : #accessing }
SpOverlayConstraints >> beOverlay [
	
	^ overlay := true
]

{ #category : #testing }
SpOverlayConstraints >> isOverlay [
	
	^ overlay = true
]
