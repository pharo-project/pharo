"
Adapt the presenter on a stub view that displays no real UI elements.
"
Class {
	#name : #SpecStubTabManagerAdapter,
	#superclass : #SpecStubAbstractAdapter,
	#category : #Spec-StubAdapter-Adapters
}

{ #category : #factory }
SpecStubTabManagerAdapter >> buildWidget [

	^ SpecStubTabManagerView new
]
