"
Adapt the presenter on a stub view that displays no real UI elements.
"
Class {
	#name : #SpecStubLabelAdapter,
	#superclass : #SpecStubAbstractAdapter,
	#category : #'Spec-StubAdapter-Adapters'
}

{ #category : #factory }
SpecStubLabelAdapter >> buildWidget [

	^ SpecStubLabelAdapter new
]
