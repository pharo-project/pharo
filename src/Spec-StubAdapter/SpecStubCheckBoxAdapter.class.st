"
Adapt the presenter on a stub view that displays no real UI elements.
"
Class {
	#name : #SpecStubCheckBoxAdapter,
	#superclass : #SpecStubAbstractAdapter,
	#category : #Spec-StubAdapter-Adapters
}

{ #category : #factory }
SpecStubCheckBoxAdapter >> buildWidget [

	^ SpecStubCheckBoxView new
]
