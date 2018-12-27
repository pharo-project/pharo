"
Adapt the presenter on a stub view that displays no real UI elements.
"
Class {
	#name : #SpecStubTableContainerAdapter,
	#superclass : #SpecStubAbstractAdapter,
	#category : #'Spec-StubAdapter-Adapters'
}

{ #category : #factory }
SpecStubTableContainerAdapter >> buildWidget [

	^ SpecStubTableContainerView new
]
