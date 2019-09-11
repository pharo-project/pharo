"
A simple inspector view that displays a morph
"
Class {
	#name : #EyeMorphViewer,
	#superclass : #EyeAbstractInspector,
	#instVars : [
		'morphPresenter'
	],
	#category : #'Spec2-Inspector-Base'
}

{ #category : #spec }
EyeMorphViewer class >> inspectorSpec [
	<spec>
	^ SpPanedLayout newVertical
		position: 70 percent;
		add: #morphPresenter;
		add: #text;
		yourself
]

{ #category : #accessing }
EyeMorphViewer class >> label [
	^ 'Morph Viewer'
]

{ #category : #initialization }
EyeMorphViewer >> initializePresenter [

	self whenWindowChanged: [ self objectChanged ]
]

{ #category : #initialization }
EyeMorphViewer >> initializeWidgets [
	super initializeWidgets.
	morphPresenter := self newMorph.

	morphPresenter
		morph:
			(PanelMorph new
				changeProportionalLayout;
				vResizing: #spaceFill;
				hResizing: #spaceFill;
				yourself)
]

{ #category : #'event-handling' }
EyeMorphViewer >> objectChanged [
	text doItReceiver: self object.
	morphPresenter morph removeAllMorphs.
	morphPresenter morph addMorph: self thumbnailMorph fullFrame: LayoutFrame identity
]

{ #category : #stepping }
EyeMorphViewer >> step [
	self objectChanged
]

{ #category : #'event-handling' }
EyeMorphViewer >> thumbnailMorph [
	^ self object taskThumbnailOfSize: self thumbnailSize
]

{ #category : #accessing }
EyeMorphViewer >> thumbnailSize [
	^ morphPresenter morph extent
]
