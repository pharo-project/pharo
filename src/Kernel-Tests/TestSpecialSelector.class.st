"
Test class that implement some special selectors used by the VM for callbacks
"
Class {
	#name : #TestSpecialSelector,
	#superclass : #Object,
	#category : #'Kernel-Tests-Special'
}

{ #category : #'reflective operations' }
TestSpecialSelector >> doesNotUnderstand: aMessage [

	^ #doesNotUnderstand: -> aMessage
]
