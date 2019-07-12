"
Adapt the presenter on a stub view that displays no real UI elements.
"
Class {
	#name : #SpStubDropListAdapter,
	#superclass : #SpStubAbstractAdapter,
	#category : #'Spec2-Adapters-Stub-Adapters'
}

{ #category : #factory }
SpStubDropListAdapter >> buildWidget [

	^ SpStubDropListView new
]
