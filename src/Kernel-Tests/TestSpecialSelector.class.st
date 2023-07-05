"
Test class that implement some special selectors used by the VM for callbacks
"
Class {
	#name : #TestSpecialSelector,
	#superclass : #Object,
	#instVars : [
		'value',
		'lastHook'
	],
	#category : #'Kernel-Tests-Special'
}

{ #category : #'reflective operations' }
TestSpecialSelector >> cannotInterpret: aMessage [

	^ #cannotInterpret: -> aMessage
]

{ #category : #'reflective operations' }
TestSpecialSelector >> doesNotUnderstand: aMessage [

	^ #doesNotUnderstand: -> aMessage
]

{ #category : #'dummy methods' }
TestSpecialSelector >> foo: anObject [
]

{ #category : #accessing }
TestSpecialSelector >> lastHook [

	^ lastHook
]

{ #category : #'block support' }
TestSpecialSelector >> mustBeBoolean [
	lastHook := #mustBeBoolean -> nil.
	^ value
]

{ #category : #accessing }
TestSpecialSelector >> value [

	^ value
]

{ #category : #accessing }
TestSpecialSelector >> value: anObject [

	value := anObject
]
