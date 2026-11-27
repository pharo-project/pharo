"
I am an abstract callback using TFFI. Used by tests
"
Class {
	#name : 'TFUFFIAbstractCallback',
	#superclass : 'FFICallback',
	#category : 'UnifiedFFI-Tests-Tests',
	#package : 'UnifiedFFI-Tests',
	#tag : 'Tests'
}

{ #category : 'accessing' }
TFUFFIAbstractCallback >> calloutAPIClass [

	^ TFCalloutAPI
]
