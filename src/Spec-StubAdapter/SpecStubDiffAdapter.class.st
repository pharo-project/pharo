"
Adapt the presenter on a stub view that displays no real UI elements.
"
Class {
	#name : #SpecStubDiffAdapter,
	#superclass : #SpecStubAbstractAdapter,
	#category : #'Spec-StubAdapter-Adapters'
}

{ #category : #factory }
SpecStubDiffAdapter >> buildWidget [

	^ SpecStubDiffView new
]
