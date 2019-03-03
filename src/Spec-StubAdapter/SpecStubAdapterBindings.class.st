"
I am used to link the Spec presenter names to the stub adapters that generate no real views.

Usefull  for images or UI managers without possible real UI output or  for tests. Can serve as template for real Spec adapters

Usage:

CurrentSpecDefaultBindings value: SpecStubAdapterBindings during: [ 
	ButtonPresenter new openWithSpec.
].
"
Class {
	#name : #SpecStubAdapterBindings,
	#superclass : #SpecAdapterBindings,
	#category : #'Spec-StubAdapter-Support'
}

{ #category : #initialize }
SpecStubAdapterBindings >> abstractAdapterClass [
	^ SpecStubAbstractAdapter
]

{ #category : #font }
SpecStubAdapterBindings >> defaultFont [

	^ StubFixedFont new
]
