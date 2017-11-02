"
Adapt the presenter on a stub view that displays no real UI elements.
"
Class {
	#name : #SpecStubTreeNodeAdapter,
	#superclass : #SpecStubAbstractAdapter,
	#category : 'Spec-StubAdapter-Adapters'
}

{ #category : #factory }
SpecStubTreeNodeAdapter >> buildWidget [

	^ SpecStubTreeNodeView new
]
