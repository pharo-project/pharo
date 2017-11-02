"
Adapt the presenter on a stub view that displays no real UI elements.
"
Class {
	#name : #SpecStubMenuGroupAdapter,
	#superclass : #SpecStubAbstractAdapter,
	#category : 'Spec-StubAdapter-Adapters'
}

{ #category : #factory }
SpecStubMenuGroupAdapter >> buildWidget [

	^ SpecStubMenuGroupView new
]
