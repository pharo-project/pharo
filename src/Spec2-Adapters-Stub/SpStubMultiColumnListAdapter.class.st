"
Adapt the presenter on a stub view that displays no real UI elements.
"
Class {
	#name : #SpStubMultiColumnListAdapter,
	#superclass : #SpStubAbstractAdapter,
	#category : #'Spec2-Adapters-Stub-Adapters'
}

{ #category : #factory }
SpStubMultiColumnListAdapter >> buildWidget [

	^ SpStubMultiColumnListView new
]
