"
Adapt the presenter on a stub view that displays no real UI elements.
"
Class {
	#name : #SpecStubMenuItemAdapter,
	#superclass : #SpecStubAbstractAdapter,
	#category : #'Spec-StubAdapter-Adapters'
}

{ #category : #factory }
SpecStubMenuItemAdapter >> buildWidget [

	^ SpecStubMenuItemView new
]
