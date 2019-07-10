"
Adapt the presenter on a stub view that displays no real UI elements.
"
Class {
	#name : #SpStubFastTableAdapter,
	#superclass : #SpStubAbstractAdapter,
	#category : #'Spec2-Adapters-Stub-Adapters'
}

{ #category : #factory }
SpStubFastTableAdapter >> buildWidget [

	^ SpStubFastTableView new
]
