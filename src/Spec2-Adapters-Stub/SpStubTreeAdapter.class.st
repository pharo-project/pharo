"
Adapt the presenter on a stub view that displays no real UI elements.
"
Class {
	#name : #SpStubTreeAdapter,
	#superclass : #SpStubAbstractAdapter,
	#category : #'Spec2-Adapters-Stub-Adapters'
}

{ #category : #factory }
SpStubTreeAdapter >> buildWidget [

	^ SpStubTreeView new
]
