"
Adapt the presenter on a stub view that displays no real UI elements.
"
Class {
	#name : #SpecStubDialogWindowAdapter,
	#superclass : #SpecStubAbstractAdapter,
	#category : 'Spec-StubAdapter-Adapters'
}

{ #category : #factory }
SpecStubDialogWindowAdapter >> buildWidget [

	^ SpecStubDialogWindowView new
]
