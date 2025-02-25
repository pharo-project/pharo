Class {
	#name : 'RGReadOnlyBackendTest',
	#superclass : 'RGBackendTest',
	#traits : 'TRGReadOnlyTest',
	#classTraits : 'TRGReadOnlyTest classTrait',
	#category : 'Ring-Core-Tests',
	#package : 'Ring-Core-Tests'
}

{ #category : 'accessing' }
RGReadOnlyBackendTest >> backendClass [

	^ RGReadOnlyBackend
]
