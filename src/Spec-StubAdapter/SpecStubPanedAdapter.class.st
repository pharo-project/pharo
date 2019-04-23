Class {
	#name : #SpecStubPanedAdapter,
	#superclass : #SpecStubLayoutAdapter,
	#category : #'Spec-StubAdapter-Adapters'
}

{ #category : #factory }
SpecStubPanedAdapter >> buildWidget [
	^ SpecStubPanedView new
]
