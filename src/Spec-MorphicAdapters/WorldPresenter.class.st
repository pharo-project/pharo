"
I am a model representing Pharo World.

This way, every spec application can be opened as the Pharo system window as window, to simulate native a application.
"
Class {
	#name : #WorldPresenter,
	#superclass : #WindowPresenter,
	#instVars : [
		'widget'
	],
	#category : #'Spec-MorphicAdapters-Base'
}

{ #category : #specs }
WorldPresenter class >> defaultSpec [
	<spec>
	
	self setUpWorld.
	^ World asSpecAdapter
]

{ #category : #private }
WorldPresenter class >> setUpWorld [
	World changeProportionalLayout.
	World submorphs do: [:e | e delete ].
 	(self environment at: #PolymorphSystemSettings) 
		perform: #desktopColor: 
		with: self theme backgroundColor.
	(self environment at: #TaskbarMorph) perform: #showTaskbar: with: false
]

{ #category : #private }
WorldPresenter >> addModelIn: container withSpec: aSpec [
	
	self model ifNil: [ ^ self ].
	widget := self model buildWithSpec: aSpec.
	
	container
		addMorph: widget
		fullFrame: LayoutFrame identity
]

{ #category : #private }
WorldPresenter >> addModelIn: container withSpecLayout: aSpec [

	self model ifNil: [ ^ self ].
	widget := self model buildWithSpecLayout: aSpec.
	
	container
		addMorph: widget
		fullFrame: LayoutFrame identity
]

{ #category : #factory }
WorldPresenter >> buildWidget [

	^ World asSpecAdapter
]

{ #category : #private }
WorldPresenter >> defaultInitialExtent [
	
	^ World extent
]

{ #category : #api }
WorldPresenter >> ensureExtentFor: aWidget [

	self extent ifNotNil: [ :ext | DisplayScreen hostWindowSize: ext ].
	self initialExtent ifNotNil: [ :ext | DisplayScreen hostWindowSize: ext ].
]

{ #category : #api }
WorldPresenter >> isDisplayed [

	^ true
]

{ #category : #private }
WorldPresenter >> openWithSpec: aSpec [

	self buildWithSpec: aSpec.
	self updateTitle
]

{ #category : #private }
WorldPresenter >> openWithSpecLayout: aSpec [

	self buildWithSpecLayout: aSpec.
	self updateTitle
]

{ #category : #private }
WorldPresenter >> title: aString [

	^ DisplayScreen hostWindowTitle: aString
]

{ #category : #private }
WorldPresenter >> updateTitle [
	"Update the window title"

	DisplayScreen hostWindowTitle: self title
]

{ #category : #api }
WorldPresenter >> widget [

	^ widget 
]
