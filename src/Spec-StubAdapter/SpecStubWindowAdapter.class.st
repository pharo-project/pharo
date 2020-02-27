"
Adapt the presenter on a stub view that displays no real UI elements.
"
Class {
	#name : #SpecStubWindowAdapter,
	#superclass : #SpecStubAbstractAdapter,
	#category : #'Spec-StubAdapter-Adapters'
}

{ #category : #factory }
SpecStubWindowAdapter >> buildWidget [

	^ SpecStubWindowView new
]

{ #category : #factory }
SpecStubWindowAdapter >> open [
]

{ #category : #factory }
SpecStubWindowAdapter >> title: aString [
]
