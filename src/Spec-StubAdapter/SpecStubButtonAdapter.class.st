"
Adapt the presenter on a stub view that displays no real UI elements.
"
Class {
	#name : #SpecStubButtonAdapter,
	#superclass : #SpecStubAbstractAdapter,
	#category : #Spec-StubAdapter-Adapters
}

{ #category : #factory }
SpecStubButtonAdapter >> buildWidget [

	^ SpecStubButtonView new
]
