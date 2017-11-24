"
Adapt the presenter on a stub view that displays no real UI elements.
"
Class {
	#name : #SpecStubIconListAdapter,
	#superclass : #SpecStubAbstractAdapter,
	#category : #'Spec-StubAdapter-Adapters'
}

{ #category : #factory }
SpecStubIconListAdapter >> buildWidget [

	^ SpecStubIconListView new
]
