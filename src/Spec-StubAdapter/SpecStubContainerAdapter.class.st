"
Adapt the presenter on a stub view that displays no real UI elements.
"
Class {
	#name : #SpecStubContainerAdapter,
	#superclass : #SpecStubAbstractAdapter,
	#category : #'Spec-StubAdapter-Adapters'
}

{ #category : #factory }
SpecStubContainerAdapter >> buildWidget [

	^ SpecStubContainerView new
]
