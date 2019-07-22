"
Adapt the presenter on a stub view that displays no real UI elements.
"
Class {
	#name : #SpStubContainerAdapter,
	#superclass : #SpStubAbstractAdapter,
	#category : #'Spec2-Adapters-Stub-Adapters'
}

{ #category : #factory }
SpStubContainerAdapter >> buildWidget [

	^ SpStubContainerView new
]
