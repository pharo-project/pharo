Class {
	#name : 'RGReadOnlyBackendTest',
	#superclass : 'RGBackendTest',
	#traits : 'TRGReadOnlyTest',
	#classTraits : 'TRGReadOnlyTest classTrait',
	#category : 'Ring-Tests-Core',
	#package : 'Ring-Tests-Core'
}

{ #category : 'accessing' }
RGReadOnlyBackendTest >> backendClass [

	^ RGReadOnlyBackend
]
