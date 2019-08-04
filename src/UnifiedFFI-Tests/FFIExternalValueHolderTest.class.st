"
Tests for FFIExternalValueHolder
"
Class {
	#name : #FFIExternalValueHolderTest,
	#superclass : #TestCase,
	#classVars : [
		'TIME_T_PTR'
	],
	#category : #'UnifiedFFI-Tests-Tests'
}

{ #category : #primitives }
FFIExternalValueHolderTest >> getTime: t [
     ^self ffiCall: #(long time(TIME_T_PTR t)) module: LibC
             
]

{ #category : #tests }
FFIExternalValueHolderTest >> testCall [
	| object time |
	
	TIME_T_PTR := FFIExternalValueHolder ofType: 'uint32'.
	object := TIME_T_PTR new.

	time := self getTime: object.
	self assert: time equals: object value
]

{ #category : #tests }
FFIExternalValueHolderTest >> testCreateValueHolder [
	| valueHolderClass object |
	
	valueHolderClass := FFIExternalValueHolder ofType: 'int'.
	object := valueHolderClass new.
	
	self assert: object type class equals: FFIInt32.
	self assert: object value equals: 0.
	
	object value: 42.
	self assert: object value equals: 42

]
