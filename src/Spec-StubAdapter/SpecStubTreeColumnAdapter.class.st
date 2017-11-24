"
Adapt the presenter on a stub view that displays no real UI elements.
"
Class {
	#name : #SpecStubTreeColumnAdapter,
	#superclass : #SpecStubAbstractAdapter,
	#category : #'Spec-StubAdapter-Adapters'
}

{ #category : #factory }
SpecStubTreeColumnAdapter >> buildWidget [

	^ SpecStubTreeColumnView new
]
