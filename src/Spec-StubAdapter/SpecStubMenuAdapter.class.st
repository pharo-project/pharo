"
Adapt the presenter on a stub view that displays no real UI elements.
"
Class {
	#name : #SpecStubMenuAdapter,
	#superclass : #SpecStubAbstractAdapter,
	#category : 'Spec-StubAdapter-Adapters'
}

{ #category : #factory }
SpecStubMenuAdapter >> buildWidget [

	^ SpecStubMenuView new
]
