"
I am an abstract callback using TFFI. Used by tests
"
Class {
	#name : #TFUFFIAbstractCallback,
	#superclass : #FFICallback,
	#category : #'ThreadedFFI-UFFI-Tests'
}

{ #category : #accessing }
TFUFFIAbstractCallback >> calloutAPIClass [

	^ TFCalloutAPI 
]
