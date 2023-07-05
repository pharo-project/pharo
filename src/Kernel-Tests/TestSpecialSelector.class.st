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

{ #category : #'write barrier' }
TestSpecialSelector >> attemptToAssign: aValue withIndex: anIndex [

	"Mutate lastHook because we cannot assign a new one in self"
	lastHook key: #attemptToAssign:withIndex:.
	lastHook value: {
			aValue.
			anIndex }

	"The return value seems ingored by the VM"
]

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

{ #category : #accessing }
TestSpecialSelector >> lastHook: anObject [

	lastHook := anObject
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
