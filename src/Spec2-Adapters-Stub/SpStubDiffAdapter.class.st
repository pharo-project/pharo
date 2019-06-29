"
Adapt the presenter on a stub view that displays no real UI elements.
"
Class {
	#name : #SpStubDiffAdapter,
	#superclass : #SpStubAbstractAdapter,
	#category : #'Spec2-Adapters-Stub-Adapters'
}

{ #category : #factory }
SpStubDiffAdapter >> buildWidget [

	^ SpStubDiffView new
]
