"
Adapt the presenter on a stub view that displays no real UI elements.
"
Class {
	#name : #SpStubButtonAdapter,
	#superclass : #SpStubAbstractAdapter,
	#category : #'Spec2-Adapters-Stub-Adapters'
}

{ #category : #factory }
SpStubButtonAdapter >> buildWidget [

	^ SpStubButtonView new
]
