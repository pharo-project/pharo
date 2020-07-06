"
Adapt the presenter on a stub view that displays no real UI elements.
"
Class {
	#name : #SpStubToolbarAdapter,
	#superclass : #SpStubAbstractAdapter,
	#category : #'Spec2-Adapters-Stub-Adapters'
}

{ #category : #factory }
SpStubToolbarAdapter >> buildWidget [

	^ SpStubToolbarView new
]
