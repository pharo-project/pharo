"
Adapt the presenter on a stub view that displays no real UI elements.
"
Class {
	#name : #SpecStubMultiColumnListAdapter,
	#superclass : #SpecStubAbstractAdapter,
	#category : #'Spec-StubAdapter-Adapters'
}

{ #category : #factory }
SpecStubMultiColumnListAdapter >> buildWidget [

	^ SpecStubMultiColumnListView new
]
