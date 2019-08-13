"
Adapt the presenter on a stub view that displays no real UI elements.
"
Class {
	#name : #SpecStubListAdapter,
	#superclass : #SpecStubAbstractAdapter,
	#category : #'Spec-StubAdapter-Adapters'
}

{ #category : #factory }
SpecStubListAdapter >> buildWidget [

	^ SpecStubListView new
]
