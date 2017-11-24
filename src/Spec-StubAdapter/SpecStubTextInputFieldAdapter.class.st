"
Adapt the presenter on a stub view that displays no real UI elements.
"
Class {
	#name : #SpecStubTextInputFieldAdapter,
	#superclass : #SpecStubAbstractAdapter,
	#category : #'Spec-StubAdapter-Adapters'
}

{ #category : #factory }
SpecStubTextInputFieldAdapter >> buildWidget [

	^ SpecStubTextInputFieldView new
]
