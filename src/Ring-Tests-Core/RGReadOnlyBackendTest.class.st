Class {
	#name : #RGReadOnlyBackendTest,
	#superclass : #RGBackendTest,
	#traits : 'TRGReadOnlyTest',
	#classTraits : 'TRGReadOnlyTest classTrait',
	#category : #'Ring-Tests-Core'
}

{ #category : #'as yet unclassified' }
RGReadOnlyBackendTest >> backendClass [

	^ RGReadOnlyBackend
]
