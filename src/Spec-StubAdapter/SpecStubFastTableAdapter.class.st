"
Adapt the presenter on a stub view that displays no real UI elements.
"
Class {
	#name : #SpecStubFastTableAdapter,
	#superclass : #SpecStubAbstractAdapter,
	#category : #Spec-StubAdapter-Adapters
}

{ #category : #factory }
SpecStubFastTableAdapter >> buildWidget [

	^ SpecStubFastTableView new
]
