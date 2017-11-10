"
Adapt the presenter on a stub view that displays no real UI elements.
"
Class {
	#name : #SpecStubDropListAdapter,
	#superclass : #SpecStubAbstractAdapter,
	#category : #Spec-StubAdapter-Adapters
}

{ #category : #factory }
SpecStubDropListAdapter >> buildWidget [

	^ SpecStubDropListView new
]
