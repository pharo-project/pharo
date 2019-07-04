Class {
	#name : #SpecStubGridAdapter,
	#superclass : #SpecStubLayoutAdapter,
	#category : #'Spec-StubAdapter-Adapters'
}

{ #category : #factory }
SpecStubGridAdapter >> buildWidget [
	^ SpecStubGridView new
]
