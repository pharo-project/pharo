"
Adapt the presenter on a stub view that displays no real UI elements.
"
Class {
	#name : #SpecStubImageAdapter,
	#superclass : #SpecStubAbstractAdapter,
	#category : #'Spec-StubAdapter-Adapters'
}

{ #category : #factory }
SpecStubImageAdapter >> buildWidget [

	^ SpecStubImageView new
]
