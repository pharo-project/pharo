"
A simple inspector view that displays a morph
"
Class {
	#name : #EyeMorphViewer,
	#superclass : #EyeAbstractInspector,
	#instVars : [
		'container'
	],
	#category : #'Spec-Inspector'
}

{ #category : #spec }
EyeMorphViewer class >> inspectorSpec [
	<spec>
	
	^ SpecLayout composed
		newRow: #container bottom: 0.3;
		addHSplitter;
		newRow: #text top: 0.7;
		yourself
]

{ #category : #accessing }
EyeMorphViewer class >> label [
	^ 'Morph Viewer'
]

{ #category : #accessing }
EyeMorphViewer >> container [
	^ container ifNil: [ container := (PanelMorph new 
		changeProportionalLayout)
		asSpecAdapter
		 vSpaceFill;
		hSpaceFill]
]

{ #category : #initialization }
EyeMorphViewer >> initializePresenter [

	self whenWindowChanged: [ self objectChanged ]
]

{ #category : #'event-handling' }
EyeMorphViewer >> objectChanged [

	self text doItReceiver: self object. 
	self container morph removeAllMorphs.
	self container morph
		addMorph: self thumbnailMorph
		fullFrame: SpecLayoutFrame identity
]

{ #category : #initialization }
EyeMorphViewer >> step [
	self objectChanged
]

{ #category : #'event-handling' }
EyeMorphViewer >> thumbnailMorph [
	^self object taskThumbnailOfSize: self thumbnailSize
]

{ #category : #accessing }
EyeMorphViewer >> thumbnailSize [
	^ self container extent
]
