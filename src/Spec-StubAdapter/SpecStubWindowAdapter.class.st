"
Adapt the presenter on a stub view that displays no real UI elements.
"
Class {
	#name : #SpecStubWindowAdapter,
	#superclass : #SpecStubAbstractAdapter,
	#category : #'Spec-StubAdapter-Adapters'
}

{ #category : #factory }
SpecStubWindowAdapter >> addContent: aView toWindow: aSpecWindow [


]

{ #category : #factory }
SpecStubWindowAdapter >> addPresenterIn: widgetToBuild withSpecLayout: aSpec [
	self
		addContent: (self model presenter buildWithSpecLayout: aSpec)
		toWindow: widgetToBuild
]

{ #category : #factory }
SpecStubWindowAdapter >> buildWidget [

	^ SpecStubWindowView new
]

{ #category : #factory }
SpecStubWindowAdapter >> close [
]

{ #category : #factory }
SpecStubWindowAdapter >> delete [
]

{ #category : #factory }
SpecStubWindowAdapter >> open [

	self presenter windowIsOpening
]

{ #category : #factory }
SpecStubWindowAdapter >> title: aString [
]
