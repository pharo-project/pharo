Class {
	#name : #SpecStubBoxAdapter,
	#superclass : #SpecStubLayoutAdapter,
	#category : #'Spec-StubAdapter-Adapters'
}

{ #category : #factory }
SpecStubBoxAdapter >> basicAdd: aPresenter constraints: constraints to: aPanel [
]

{ #category : #factory }
SpecStubBoxAdapter >> buildWidget [
	^ SpecStubBoxView new
]
