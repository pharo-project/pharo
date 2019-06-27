"
Adapt the presenter on a stub view that displays no real UI elements.
"
Class {
	#name : #SpStubMenuGroupAdapter,
	#superclass : #SpStubAbstractAdapter,
	#category : #'Spec2-Adapters-Stub-Adapters'
}

{ #category : #factory }
SpStubMenuGroupAdapter >> buildWidget [

	^ SpStubMenuGroupView new
]
