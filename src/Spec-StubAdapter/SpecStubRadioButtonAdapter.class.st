"
Adapt the presenter on a stub view that displays no real UI elements.
"
Class {
	#name : #SpecStubRadioButtonAdapter,
	#superclass : #SpecStubAbstractAdapter,
	#category : 'Spec-StubAdapter-Adapters'
}

{ #category : #factory }
SpecStubRadioButtonAdapter >> buildWidget [

	^ SpecStubRadioButtonView new
]
