"
Adapt the presenter on a stub view that displays no real UI elements.
"
Class {
	#name : #SpStubWindowAdapter,
	#superclass : #SpStubAbstractAdapter,
	#category : #'Spec2-Adapters-Stub-Adapters'
}

{ #category : #factory }
SpStubWindowAdapter >> addContent: aView toWindow: aSpecWindow [


]

{ #category : #factory }
SpStubWindowAdapter >> addPresenterIn: widgetToBuild withSpecLayout: aSpec [
	self
		addContent: (self model presenter buildWithSpecLayout: aSpec)
		toWindow: widgetToBuild
]

{ #category : #factory }
SpStubWindowAdapter >> beep [
]

{ #category : #factory }
SpStubWindowAdapter >> buildWidget [

	^ SpStubWindowView new
]

{ #category : #factory }
SpStubWindowAdapter >> close [
]

{ #category : #factory }
SpStubWindowAdapter >> delete [
]

{ #category : #factory }
SpStubWindowAdapter >> open [

	self presenter windowIsOpening
]

{ #category : #factory }
SpStubWindowAdapter >> title: aString [
]
