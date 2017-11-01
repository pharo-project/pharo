"
Adapt the presenter on a stub view that displays no real UI elements.
"
Class {
	#name : #SpecStubTickingWindowAdapter,
	#superclass : #SpecStubAbstractAdapter,
	#category : 'Spec-StubAdapter-Adapters'
}

{ #category : #factory }
SpecStubTickingWindowAdapter >> buildWidget [

	^ SpecStubTickingWindowView new
]
