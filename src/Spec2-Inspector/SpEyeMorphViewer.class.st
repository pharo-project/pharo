"
A simple inspector view that displays a morph
"
Class {
	#name : #SpEyeMorphViewer,
	#superclass : #SpEyeAbstractInspector,
	#instVars : [
		'container'
	],
	#category : #'Spec2-Inspector-Base'
}

{ #category : #spec }
SpEyeMorphViewer class >> inspectorSpec [
	<spec>
	
	^ SpLayout composed
		newRow: #container bottom: 0.3;
		addHSplitter;
		newRow: #text top: 0.7;
		yourself
]

{ #category : #accessing }
SpEyeMorphViewer class >> label [
	^ 'Morph Viewer'
]

{ #category : #accessing }
SpEyeMorphViewer >> container [
	^ container ifNil: [ container := (PanelMorph new 
		changeProportionalLayout)
		asSpAdapter
		 vSpaceFill;
		hSpaceFill]
]

{ #category : #initialization }
SpEyeMorphViewer >> initializePresenter [

	self whenWindowChanged: [ self objectChanged ]
]

{ #category : #'event-handling' }
SpEyeMorphViewer >> objectChanged [

	self text doItReceiver: self object. 
	self container morph removeAllMorphs.
	self container morph
		addMorph: self thumbnailMorph
		fullFrame: SpLayoutFrame identity
]

{ #category : #initialization }
SpEyeMorphViewer >> step [
	self objectChanged
]

{ #category : #'event-handling' }
SpEyeMorphViewer >> thumbnailMorph [
	^self object taskThumbnailOfSize: self thumbnailSize
]

{ #category : #accessing }
SpEyeMorphViewer >> thumbnailSize [
	^ self container extent
]
