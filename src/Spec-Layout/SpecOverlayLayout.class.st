Class {
	#name : #SpecOverlayLayout,
	#superclass : #SpecExecutableLayout,
	#category : #'Spec-Layout-Overlay'
}

{ #category : #private }
SpecOverlayLayout >> adapterName [
	
	^ #OverlayAdapter
]

{ #category : #accessing }
SpecOverlayLayout >> add: aName [

	self
		add: aName 
		withConstraints: [ :constraints | ]
]

{ #category : #private }
SpecOverlayLayout >> constraintsClass [
	
	^ SpecOverlayConstraints
]
