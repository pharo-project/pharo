"
Adapt the presenter on a stub view that displays no real UI elements.
"
Class {
	#name : #SpecStubTabAdapter,
	#superclass : #SpecStubAbstractAdapter,
	#category : #'Spec-StubAdapter-Adapters'
}

{ #category : #factory }
SpecStubTabAdapter >> buildWidget [

	^ SpecStubTabView new
]
