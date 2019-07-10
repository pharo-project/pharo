"
Adapt the presenter on a stub view that displays no real UI elements.
"
Class {
	#name : #SpecStubNumberInputFieldAdapter,
	#superclass : #SpecStubAbstractAdapter,
	#category : #'Spec-StubAdapter-Adapters'
}

{ #category : #factory }
SpecStubNumberInputFieldAdapter >> buildWidget [
	^ SpecStubNumberInputFieldView new
]
