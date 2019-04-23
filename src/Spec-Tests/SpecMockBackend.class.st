Class {
	#name : #SpecMockBackend,
	#superclass : #SpecApplicationBackend,
	#category : #'Spec-Tests-Core-Base'
}

{ #category : #accessing }
SpecMockBackend class >> backendName [

	^ #Mock
]

{ #category : #private }
SpecMockBackend >> adapterBindingsClass [

	^ SpecStubAdapterBindings
]
