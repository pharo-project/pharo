"
Adapt the presenter on a stub view that displays no real UI elements.
"
Class {
	#name : #SpecStubTreeAdapter,
	#superclass : #SpecStubAbstractAdapter,
	#category : #'Spec-StubAdapter-Adapters'
}

{ #category : #factory }
SpecStubTreeAdapter >> buildWidget [

	^ SpecStubTreeView new
]
