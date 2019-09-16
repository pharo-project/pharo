"
I am a model representing Pharo World.

This way, every spec application can be opened as the Pharo system window as window, to simulate native a application.
"
Class {
	#name : #SpWorldPresenter,
	#superclass : #SpWindowPresenter,
	#instVars : [
		'widget'
	],
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #specs }
SpWorldPresenter class >> defaultSpec [
	self setUpWorld.
	^ SpMorphPresenter new
		morph: self currentWorld;
		yourself
]

{ #category : #private }
SpWorldPresenter class >> setUpWorld [

	self currentWorld changeProportionalLayout.
	self currentWorld submorphs do: [:e | e delete ].
 
 	(Smalltalk at: #PolymorphSystemSettings) 
		perform: #desktopColor: 
		with: self currentWorld theme backgroundColor.
 	"((Smalltalk at: #UITheme) perform: #currentSettings) perform: #windowColor: with: (Color r: 0.745 g: 0.745 b: 0.745)."
	(Smalltalk at: #TaskbarMorph) perform: #showTaskbar: with: false
]

{ #category : #private }
SpWorldPresenter >> addPresenterIn: container withSpecLayout: aSpec [

	self presenter ifNil: [ ^ self ].
	widget := self presenter buildWithSpecLayout: aSpec.
	
	container
		addMorph: widget
		fullFrame: LayoutFrame identity
]

{ #category : #factory }
SpWorldPresenter >> buildWidget [
	^ SpMorphPresenter new
		morph: self currentWorld;
		yourself
]

{ #category : #private }
SpWorldPresenter >> defaultInitialExtent [
	
	^ self currentWorld extent
]

{ #category : #api }
SpWorldPresenter >> ensureExtentFor: aWidget [

	self extent ifNotNil: [ :ext | DisplayScreen hostWindowSize: ext ].
	self initialExtent ifNotNil: [ :ext | DisplayScreen hostWindowSize: ext ].
]

{ #category : #api }
SpWorldPresenter >> isDisplayed [

	^ true
]

{ #category : #private }
SpWorldPresenter >> openWithSpec: aSpec [

	self buildWithSpec: aSpec.
	self updateTitle
]

{ #category : #private }
SpWorldPresenter >> openWithSpecLayout: aSpec [

	self buildWithSpecLayout: aSpec.
	self updateTitle
]

{ #category : #private }
SpWorldPresenter >> title: aString [

	^ DisplayScreen hostWindowTitle: aString
]

{ #category : #private }
SpWorldPresenter >> updateTitle [
	"Update the window title"

	DisplayScreen hostWindowTitle: self title
]

{ #category : #api }
SpWorldPresenter >> widget [

	^ widget 
]
