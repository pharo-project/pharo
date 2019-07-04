Class {
	#name : #SpMockBackend,
	#superclass : #SpApplicationBackend,
	#category : #'Spec2-Tests-Core-Base'
}

{ #category : #accessing }
SpMockBackend class >> backendName [

	^ #Mock
]

{ #category : #private }
SpMockBackend >> adapterBindingsClass [

	^ SpStubAdapterBindings
]
