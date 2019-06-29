"
Adapt the presenter on a stub view that displays no real UI elements.
"
Class {
	#name : #SpStubTreeNodeAdapter,
	#superclass : #SpStubAbstractAdapter,
	#category : #'Spec2-Adapters-Stub-Adapters'
}

{ #category : #factory }
SpStubTreeNodeAdapter >> buildWidget [

	^ SpStubTreeNodeView new
]
